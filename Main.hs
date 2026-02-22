{-# LANGUAGE GADTs #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Codec.Picture (Image (imageData), PixelRGBA8 (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Data.Binary hiding (get, put)
import Data.Binary qualified as Bin
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Internal qualified as BSL
import Data.Int
import Data.Maybe (fromJust)
import Data.Typeable
import Events
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (loadFontFile)
import Headers
import Network.Socket
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)
import System.Environment (getEnv)
import System.Posix (setFdSize)
import System.Posix.IO
import System.Posix.SharedMem
import System.Posix.Types
import Text.Printf
import Utils

headerSize :: Int64
headerSize = 8

waylandNull :: Word32
waylandNull = 0

wlDisplayID :: Word32
wlDisplayID = 1 -- wlDisplay always has ID 1 in the protocol

type Wayland = ReaderT WaylandEnv IO

data WaylandEnv = WaylandEnv
  { tracker :: IORef ObjectTracker
  , socket :: Socket
  , counter :: IORef Word32
  , registryID :: Word32
  , wl_shmID :: Word32
  , wl_compositorID :: Word32
  , zwlr_layer_shell_v1ID :: Word32
  , parseEvents :: ObjectTracker -> Get [WaylandEvent]
  }

nextID' :: IORef Word32 -> IO Word32
nextID' counter = do
  current <- readIORef counter
  modifyIORef counter (+ 1)
  return current

nextID :: IORef Word32 -> Wayland Word32
nextID = liftIO . nextID'

mkMessage :: Word32 -> Word16 -> ByteString -> ByteString
mkMessage objectID opCode messageBody =
  runPut $ do
    putWord32le objectID
    putWord16le opCode
    putWord16le $ fromIntegral (headerSize + length messageBody)
    putLazyByteString messageBody

wlDisplayConnect :: IO Socket
wlDisplayConnect = do
  xdg_runtime_dir <- getEnv "XDG_RUNTIME_DIR"
  wayland_display <- getEnv "WAYLAND_DISPLAY"
  let path = xdg_runtime_dir <> "/" <> wayland_display
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  return sock

receiveSocketData :: Socket -> IO ByteString
receiveSocketData sock = do
  liftIO $ recv sock 4096

parseEvent :: Word32 -> Maybe Word32 -> ObjectTracker -> Get WaylandEvent
parseEvent registryID wl_shmID tracker = do
  header <- Bin.get
  let matchEvent' = matchEvent header
  let maybeMatchEvent' = maybeMatchEvent header
  let bodySize = fromIntegral header.size - 8
  let ev :: (Binary a, WaylandEventType a, Typeable a) => Get a -> Get WaylandEvent
      ev = fmap (Event header)
  case (header.objectID, header.opCode) of
    -- wl_display events (always object 1)
    (1, 0) -> ev (Bin.get @EventDisplayError)
    (1, 1) -> ev (Bin.get @EventDisplayDeleteId)
    _
      | matchEvent' registryID 0 -> ev (Bin.get @EventGlobal)
      | matchEvent' registryID 1 -> skip bodySize $> EvUnknown header
      | maybeMatchEvent' wl_shmID 0 -> ev (Bin.get @EventShmFormat)
      | maybeMatchEvent' tracker.zwlr_layer_surface_v1ID 0 -> ev (Bin.get @EventWlrLayerSurfaceConfigure)
      | otherwise -> skip bodySize $> EvUnknown header
  where
    maybeMatchEvent :: Header -> Maybe Word32 -> Word16 -> Bool
    maybeMatchEvent header (Just oid) opcode = matchEvent header oid opcode
    maybeMatchEvent _ Nothing _ = False

    matchEvent :: Header -> Word32 -> Word16 -> Bool
    matchEvent header oid opcode = oid == header.objectID && header.opCode == opcode

parseEvents :: Word32 -> Maybe Word32 -> ObjectTracker -> Get [WaylandEvent]
parseEvents registryID wl_shmID tracker = do
  isEmpty >>= \case
    True -> return []
    False -> (:) <$> parseEvent registryID wl_shmID tracker <*> parseEvents registryID wl_shmID tracker

wlDisplay_getRegistry :: Socket -> IORef Word32 -> IO Word32
wlDisplay_getRegistry sock counter = do
  registryID <- nextID' counter
  let messageBody = runPut $ putWord32le registryID
  liftIO $ sendAll sock $ mkMessage wlDisplayID 1 messageBody
  putStrLn $ printf "  --> wl_display@1.get_registry: wl_registry=%i" registryID
  pure registryID

wlRegistry_bind :: Socket -> Word32 -> Word32 -> ByteString -> Word32 -> Word32 -> IO Word32
wlRegistry_bind sock registryID globalName interfaceName interfaceVersion newObjectID = do
  let messageBody = runPut $ do
        putWord32le globalName
        putWlString interfaceName
        putWord32le interfaceVersion
        putWord32le newObjectID
  liftIO $ sendAll sock $ mkMessage registryID 0 messageBody

  putStrLn
    $ printf
      " --> wl_registry@%i.bind: name=%i interface=\"%s\" version=%i id=%i"
      registryID
      globalName
      (BSL.unpackChars interfaceName)
      interfaceVersion
      newObjectID
  pure newObjectID

wlCompositor_createSurface :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Wayland ()
wlCompositor_createSurface updateFn = do
  env <- ask
  newObjectID <- nextID env.counter

  let messageBody = runPut $ putWord32le newObjectID
  liftIO $ sendAll env.socket $ mkMessage env.wl_compositorID 0 messageBody

  putStrLn $ printf " --> wl_compositor@%i.create_surface: newID=%i" env.wl_compositorID newObjectID
  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

wlSurface_commit :: Wayland ()
wlSurface_commit = do
  env <- ask
  wl_surfaceID <- fromJust . (.wl_surfaceID) <$> readIORef env.tracker

  let messageBody = runPut mempty
  liftIO . sendAll env.socket $ mkMessage wl_surfaceID 6 messageBody

  putStrLn $ printf " --> wl_surface@%i.commit: commit request" wl_surfaceID

wlSurface_attach :: Wayland ()
wlSurface_attach = do
  env <- ask
  tracker <- readIORef env.tracker
  let wl_surfaceID = fromJust tracker.wl_surfaceID
  let wl_bufferID = fromJust tracker.wl_bufferID

  let messageBody = runPut $ do
        -- putWord32le wl_bufferID
        putWord32le wl_bufferID
        -- x y arguments have to be set to 0
        putInt32le 0
        putInt32le 0
  liftIO . sendAll env.socket $ mkMessage wl_surfaceID 1 messageBody

  putStrLn $ printf " --> wl_surface@%i.attach: bufferId=%i x=%i y=%i" wl_surfaceID wl_bufferID 0 0

wlShm_createPool :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Fd -> Word32 -> Wayland ()
wlShm_createPool updateFn fileDescriptor size = do
  env <- ask
  newObjectID <- nextID env.counter
  let wl_shmID = env.wl_shmID
  let messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le size
  let msg = BS.toStrict $ mkMessage wl_shmID 0 messageBody
  liftIO $ sendManyWithFds env.socket [msg] [fileDescriptor]

  putStrLn $ printf " --> wl_shm@%i.create_pool: newID=%i fd=%s size=%i" wl_shmID newObjectID (show fileDescriptor :: Text) size
  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

zwlrLayerShellV1_getLayerSurface :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> ByteString -> Wayland ()
zwlrLayerShellV1_getLayerSurface updateFn layer namespace = do
  env <- ask
  tracker <- readIORef env.tracker
  newObjectID <- nextID env.counter
  let zwlr_layer_shell_v1ID = env.zwlr_layer_shell_v1ID
  let wl_surfaceID = fromJust tracker.wl_surfaceID

  let messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le wl_surfaceID
        putWord32le waylandNull
        putWord32le layer
        putWlString namespace
  liftIO . sendAll env.socket $ mkMessage zwlr_layer_shell_v1ID 0 messageBody

  putStrLn $ printf " --> zwlr_layer_shell_v1@%i.get_layer_surface: newID=%i wl_surface=%i output=%i layer=%i namespace=%s" zwlr_layer_shell_v1ID newObjectID wl_surfaceID waylandNull layer (BSL.unpackChars namespace)
  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

zwlrLayerSurfaceV1_setAnchor :: Word32 -> Wayland ()
zwlrLayerSurfaceV1_setAnchor anchor = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ putWord32le anchor
  liftIO $ sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 1 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.set_anchor: anchor=%i" zwlr_layer_surface_v1ID anchor

zwlrLayerSurfaceV1_setSize :: Word32 -> Word32 -> Wayland ()
zwlrLayerSurfaceV1_setSize width height = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ do
        putWord32le width
        putWord32le height
  liftIO . sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 0 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.set_size: width=%i height=%i" zwlr_layer_surface_v1ID width height

zwlrLayerSurfaceV1_ackConfigure :: Wayland ()
zwlrLayerSurfaceV1_ackConfigure = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID
  let zwlr_layer_surface_v1Serial = fromJust tracker.zwlr_layer_surface_v1Serial

  let messageBody = runPut $ do putWord32le zwlr_layer_surface_v1Serial
  liftIO . sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 6 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.ack_configure: serial=%i" zwlr_layer_surface_v1ID zwlr_layer_surface_v1Serial

zwlrLayerSurfaceV1_setExclusiveZone :: Int32 -> Wayland ()
zwlrLayerSurfaceV1_setExclusiveZone zone = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ do putInt32le zone
  liftIO $ sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 2 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.set_exclusive_zone: zone=%i" zwlr_layer_surface_v1ID zone

wlShmPool_createBuffer :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Wayland ()
wlShmPool_createBuffer updateFn offset width height stride format = do
  env <- ask
  tracker <- readIORef env.tracker
  newObjectID <- nextID env.counter
  let wl_shm_poolID = fromJust tracker.wl_shm_poolID

  let messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le offset
        putWord32le width
        putWord32le height
        putWord32le stride
        putWord32le format
  liftIO . sendAll env.socket $ mkMessage wl_shm_poolID 0 messageBody

  putStrLn $ printf " --> wl_shm_pool@%i.create_buffer: newID=%i" wl_shm_poolID newObjectID
  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

findInterface :: [(Header, EventGlobal)] -> ByteString -> Maybe EventGlobal
findInterface messages targetInterface =
  let target = targetInterface <> "\0"
   in Relude.find (\(_, e) -> target `isPrefixOf` e.interface) messages >>= Just . snd

bindToInterface :: Socket -> Word32 -> IORef Word32 -> [(Header, EventGlobal)] -> ByteString -> IO Word32
bindToInterface sock registryID counterRef globals targetInterface =
  case findInterface globals targetInterface of
    Nothing -> error ("ERROR: " <> toText (BSL.unpackChars targetInterface) <> " not found")
    Just e -> do
      newObjectID <- nextID' counterRef
      wlRegistry_bind sock registryID e.name targetInterface e.version newObjectID

eventLoop :: Wayland ()
eventLoop = do
  env <- ask
  msg <- liftIO $ receiveSocketData env.socket
  tracker <- readIORef env.tracker
  unless (BSL.null msg) $ do
    let events = runGet (env.parseEvents tracker) msg
    forM_ events $ \event -> do
      liftIO $ displayEvent event
      handleEventResponse event -- Handle events that need responses
  eventLoop

handleEventResponse :: WaylandEvent -> Wayland ()
handleEventResponse (Event _h e) = do
  env <- ask
  whenJust (cast e) $ \(configure :: EventWlrLayerSurfaceConfigure) -> modifyIORef' env.tracker (\t -> t{zwlr_layer_surface_v1Serial = Just configure.serial})
handleEventResponse _ = return ()

data ObjectTracker = ObjectTracker
  { wl_surfaceID :: Maybe Word32
  , wl_shm_poolID :: Maybe Word32
  , wl_bufferID :: Maybe Word32
  , zwlr_layer_surface_v1ID :: Maybe Word32
  , zwlr_layer_surface_v1Serial :: Maybe Word32
  }

main :: IO ()
main = runReaderT program =<< waylandSetup

waylandSetup :: IO WaylandEnv
waylandSetup = do
  sock <- wlDisplayConnect
  counter <- newIORef 2 -- start from 2 because wl_display is always 1
  registry <- wlDisplay_getRegistry sock counter
  socketData <- receiveSocketData sock
  tracker <- newIORef (ObjectTracker Nothing Nothing Nothing Nothing Nothing)
  initialEvents <- runGet . parseEvents registry Nothing <$> readIORef tracker <*> pure socketData
  mapM_ displayEvent initialEvents
  let globals = [(h, g) | ev <- initialEvents, Event h e <- [ev], Just g <- [cast e :: Maybe EventGlobal]]

  putStrLn "\n--- Binding to interfaces ---"
  wl_shm <- bindToInterface sock registry counter globals "wl_shm"
  wl_compositor <- bindToInterface sock registry counter globals "wl_compositor"
  zwlr_layer_shell_v1 <- bindToInterface sock registry counter globals "zwlr_layer_shell_v1"

  let eventParser = parseEvents registry (Just wl_shm)

  pure
    $ WaylandEnv
      tracker
      sock
      counter
      registry
      wl_shm
      wl_compositor
      zwlr_layer_shell_v1
      eventParser

program :: Wayland ()
program = do
  env <- ask
  let colorChannels :: Word32 = 4
      bufferWidth :: Word32 = 1920
      bufferHeight :: Word32 = 25
      stride :: Word32 = bufferWidth * colorChannels
      colorFormat :: Word32 = 0 -- ARGB8888
      sharedPoolSize :: Word32 = bufferWidth * bufferHeight * colorChannels
      poolName :: String = "saybar-shared-pool"

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT eventLoop env)
      (close env.socket)

  liftIO $ threadDelay 100000

  wlCompositor_createSurface $ \t objectID -> t{wl_surfaceID = objectID}
  zwlrLayerShellV1_getLayerSurface (\t objectID -> t{zwlr_layer_surface_v1ID = objectID}) 2 "saybar"
  zwlrLayerSurfaceV1_setAnchor 13 -- top left right anchors
  zwlrLayerSurfaceV1_setSize 0 bufferHeight -- top left right anchors
  zwlrLayerSurfaceV1_setExclusiveZone (fromIntegral bufferHeight)
  wlSurface_commit
  liftIO $ threadDelay 100000 -- Wait for response from commit
  zwlrLayerSurfaceV1_ackConfigure

  font <- either (error . toText) pure =<< liftIO (loadFontFile "CourierPrime-Regular.ttf")
  let bgColor = PixelRGBA8 0 0 0 0
      drawColor = PixelRGBA8 213 196 161 255 -- #d5c4a1
      img = renderDrawing (fromIntegral bufferWidth) (fromIntegral bufferHeight) bgColor $ do
        withTexture (uniformTexture drawColor) $ do
          printTextAt font (PointSize 12) (V2 20 15) "date: 2026-02-18 21:13. internet: connected. tray: steam and discord open. Data last updated at compile time with my keyboard"

  liftIO
    . void
    $ bracket
      (shmOpen poolName (ShmOpenFlags True True True True) 0600)
      (\_fd -> shmUnlink poolName)
      ( \fileDescriptor -> flip runReaderT env $ do
          liftIO $ setFdSize fileDescriptor (fromIntegral sharedPoolSize)
          wlShm_createPool (\t objectID -> t{wl_shm_poolID = objectID}) fileDescriptor sharedPoolSize
          _wl_buffer <- wlShmPool_createBuffer (\t objectID -> t{wl_bufferID = objectID}) 0 bufferWidth bufferHeight stride colorFormat

          file_handle <- liftIO $ fdToHandle fileDescriptor

          let pixelData = swizzleRGBAtoBGRA img
          liftIO $ hPut file_handle pixelData

          wlSurface_attach
          wlSurface_commit
          liftIO $ threadDelay maxBound
      )
