{-# LANGUAGE GADTs #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Codec.Picture (Image (imageData), PixelRGBA8 (..), writePng)
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
import Data.Vector.Storable qualified as VS
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
  { socket :: Socket
  , counter :: IORef Word32
  , tracker :: IORef ObjectTracker
  }

nextID :: IORef Word32 -> Wayland Word32
nextID counter = do
  current <- readIORef counter
  modifyIORef counter (+ 1)
  return current

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

receiveSocketData :: Wayland ByteString
receiveSocketData = do
  env <- ask
  liftIO $ recv env.socket 4096

parseEvent :: ObjectTracker -> Get WaylandEvent
parseEvent tracker = do
  header <- Bin.get
  let matchEvent' = matchEvent header
  let bodySize = fromIntegral header.size - 8
  let ev :: (Binary a, WaylandEventType a, Typeable a) => Get a -> Get WaylandEvent
      ev = fmap (Event header)
  case (header.objectID, header.opCode) of
    -- wl_display events (always object 1)
    (1, 0) -> ev (Bin.get @EventDisplayError)
    (1, 1) -> ev (Bin.get @EventDisplayDeleteId)
    _
      | matchEvent' tracker.registryID 0 -> ev (Bin.get @EventGlobal)
      | matchEvent' tracker.registryID 1 -> skip bodySize $> EvUnknown header
      | matchEvent' tracker.wl_shmID 0 -> ev (Bin.get @EventShmFormat)
      | matchEvent' tracker.zwlr_layer_surface_v1ID 0 -> ev (Bin.get @EventWlrLayerSurfaceConfigure)
      | otherwise -> skip bodySize $> EvUnknown header
  where
    matchEvent :: Header -> Maybe Word32 -> Word16 -> Bool
    matchEvent header (Just oid) opcode = oid == header.objectID && header.opCode == opcode
    matchEvent _ Nothing _ = False

parseEvents :: ObjectTracker -> Get [WaylandEvent]
parseEvents tracker = do
  isEmpty >>= \case
    True -> return []
    False -> (:) <$> parseEvent tracker <*> parseEvents tracker

wlDisplay_getRegistry :: Wayland ()
wlDisplay_getRegistry = do
  env <- ask
  newObjectID <- nextID env.counter
  let messageBody = runPut $ putWord32le newObjectID
  liftIO $ sendAll env.socket $ mkMessage wlDisplayID 1 messageBody
  putStrLn $ printf "  --> wl_display@1.get_registry: wl_registry=%i" newObjectID
  modifyIORef' env.tracker $ \t -> t{registryID = Just newObjectID}

wlRegistry_bind :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> ByteString -> Word32 -> Wayland ()
wlRegistry_bind updateFn globalName interfaceName interfaceVersion = do
  env <- ask
  registryID <- fromJust . (.registryID) <$> readIORef env.tracker
  newObjectID <- nextID env.counter
  let messageBody = runPut $ do
        putWord32le globalName
        putWlString interfaceName
        putWord32le interfaceVersion
        putWord32le newObjectID
  liftIO $ sendAll env.socket $ mkMessage registryID 0 messageBody

  putStrLn
    $ printf
      " --> wl_registry@%i.bind: name=%i interface=\"%s\" version=%i id=%i"
      registryID
      globalName
      (BSL.unpackChars interfaceName)
      interfaceVersion
      newObjectID
  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

wlCompositor_createSurface :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Wayland ()
wlCompositor_createSurface updateFn = do
  env <- ask
  wl_compositorID <- fromJust . (.wl_compositorID) <$> readIORef env.tracker
  newObjectID <- nextID env.counter

  let messageBody = runPut $ putWord32le newObjectID
  liftIO $ sendAll env.socket $ mkMessage wl_compositorID 0 messageBody

  putStrLn $ printf " --> wl_compositor@%i.create_surface: newID=%i" wl_compositorID newObjectID
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
  tracker <- readIORef env.tracker
  newObjectID <- nextID env.counter
  let wl_shmID = fromJust tracker.wl_shmID
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
  let zwlr_layer_shell_v1ID = fromJust tracker.zwlr_layer_shell_v1ID
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

bindToInterface :: [(Header, EventGlobal)] -> ByteString -> (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Wayland ()
bindToInterface globals targetInterface updateFn =
  case findInterface globals targetInterface of
    Nothing -> putStrLn ("ERROR: " <> BSL.unpackChars targetInterface <> " not found")
    Just e -> wlRegistry_bind updateFn e.name targetInterface e.version

eventLoop :: Wayland ()
eventLoop = do
  env <- ask
  msg <- receiveSocketData
  tracker <- readIORef env.tracker
  unless (BSL.null msg) $ do
    let events = runGet (parseEvents tracker) msg
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
  { registryID :: Maybe Word32
  , wl_shmID :: Maybe Word32
  , wl_compositorID :: Maybe Word32
  , wl_surfaceID :: Maybe Word32
  , wl_shm_poolID :: Maybe Word32
  , wl_bufferID :: Maybe Word32
  , zwlr_layer_shell_v1ID :: Maybe Word32
  , zwlr_layer_surface_v1ID :: Maybe Word32
  , zwlr_layer_surface_v1Serial :: Maybe Word32
  }

main :: IO ()
main = do
  env <- WaylandEnv <$> wlDisplayConnect <*> newIORef 2 <*> newIORef (ObjectTracker Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  runReaderT program env

program :: Wayland ()
program = do
  env <- ask
  wlDisplay_getRegistry
  socketData <- receiveSocketData
  initialEvents <- runGet . parseEvents <$> readIORef env.tracker <*> pure socketData
  liftIO $ mapM_ displayEvent initialEvents

  -- Extract globals from events
  let globals = [(h, g) | ev <- initialEvents, Event h e <- [ev], Just g <- [cast e :: Maybe EventGlobal]]

  let colorChannels :: Word32 = 4
  let bufferWidth :: Word32 = 1920
  let bufferHeight :: Word32 = 25
  let stride :: Word32 = bufferWidth * colorChannels
  let colorFormat :: Word32 = 0 -- ARGB8888
  let sharedPoolSize :: Word32 = bufferWidth * bufferHeight * colorChannels
  let poolName :: String = "saybar-shared-pool"

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT eventLoop env)
      (close env.socket)

  liftIO $ threadDelay 100000

  -- Bind to the required interfaces
  putStrLn "\n--- Binding to interfaces ---"
  bindToInterface globals "wl_shm" $ \t objectID -> t{wl_shmID = objectID}
  bindToInterface globals "wl_compositor" $ \t objectID -> t{wl_compositorID = objectID}
  bindToInterface globals "zwlr_layer_shell_v1" $ \t objectID -> t{zwlr_layer_shell_v1ID = objectID}

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

          let swizzleRGBAtoBGRA :: Image PixelRGBA8 -> BS.ByteString
              swizzleRGBAtoBGRA image =
                BS.pack . go . VS.toList $ imageData image
                where
                  go [] = []
                  go (r : g : b : a : rest) =
                    let premul c = fromIntegral (fromIntegral c * fromIntegral a `div` 255 :: Word16)
                     in premul b : premul g : premul r : a : go rest
                  go _ = []

          let pixelData = swizzleRGBAtoBGRA img
          liftIO $ BS.hPut file_handle pixelData

          wlSurface_attach
          wlSurface_commit
          liftIO $ threadDelay maxBound
      )
