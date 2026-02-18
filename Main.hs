{-# LANGUAGE GADTs #-}

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

nextID :: IORef Word32 -> IO Word32
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

receiveSocketData :: Socket -> IO ByteString
receiveSocketData sock = recv sock 4096

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

wlDisplayGetRegistry :: Socket -> Word32 -> IO Word32
wlDisplayGetRegistry wl_display newObjectID = do
  let messageBody = runPut $ putWord32le newObjectID
  sendAll wl_display $ mkMessage 1 1 messageBody
  putStrLn "  --> wl_display@1.get_registry: wl_registry=2"
  return newObjectID

wlRegistryBind :: Socket -> Word32 -> IORef ObjectTracker -> (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> ByteString -> Word32 -> Word32 -> IO Word32
wlRegistryBind sock registryID trackerRef updateFn globalName interfaceName interfaceVersion newObjectID = do
  let interfaceStr = interfaceName <> "\0" -- Null-terminated string
  let interfaceLen = fromIntegral $ length interfaceStr
  let paddingBytes = padLen interfaceLen - fromIntegral interfaceLen

  let messageBody = runPut $ do
        putWord32le globalName -- name
        putWord32le interfaceLen -- string length
        putLazyByteString interfaceStr -- interface string
        replicateM_ (fromIntegral paddingBytes) (putWord8 0) -- padding
        putWord32le interfaceVersion -- version
        putWord32le newObjectID -- new_id
  sendAll sock $ mkMessage registryID 0 messageBody

  putStrLn
    $ printf
      " --> wl_registry@%i.bind: name=%i interface=\"%s\" version=%i id=%i"
      registryID
      globalName
      (BSL.unpackChars interfaceName)
      interfaceVersion
      newObjectID
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

wlCompositorCreateSurface :: Socket -> IORef ObjectTracker -> (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> IO Word32
wlCompositorCreateSurface sock trackerRef updateFn newObjectID = do
  tracker <- readIORef trackerRef
  let wl_compositorID = fromJust tracker.wl_compositorID

  let messageBody = runPut $ putWord32le newObjectID
  sendAll sock $ mkMessage wl_compositorID 0 messageBody

  putStrLn $ printf " --> wl_compositor@%i.create_surface: newID=%i" wl_compositorID newObjectID
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

wlSurfaceCommit :: Socket -> IORef ObjectTracker -> IO ()
wlSurfaceCommit sock trackerRef = do
  tracker <- readIORef trackerRef
  let wl_surfaceID = fromJust tracker.wl_surfaceID

  let messageBody = runPut mempty
  sendAll sock $ mkMessage wl_surfaceID 6 messageBody

  putStrLn $ printf " --> wl_surface@%i.commit: commit request" wl_surfaceID

wlSurfaceAttach :: Socket -> IORef ObjectTracker -> IO ()
wlSurfaceAttach sock trackerRef = do
  tracker <- readIORef trackerRef
  let wl_surfaceID = fromJust tracker.wl_surfaceID
  let wl_bufferID = fromJust tracker.wl_bufferID

  let messageBody = runPut $ do
        -- putWord32le wl_bufferID
        putWord32le wl_bufferID
        -- x y arguments have to be set to 0
        putInt32le 0
        putInt32le 0
  sendAll sock $ mkMessage wl_surfaceID 1 messageBody

  putStrLn $ printf " --> wl_surface@%i.attach: bufferId=%i x=%i y=%i" wl_surfaceID wl_bufferID 0 0

wlShmCreatePool :: Socket -> IORef ObjectTracker -> (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Fd -> Word32 -> Word32 -> IO Word32
wlShmCreatePool sock trackerRef updateFn fileDescriptor size newObjectID = do
  tracker <- readIORef trackerRef
  let wl_shmID = fromJust tracker.wl_shmID
  let messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le size
  let msg = BS.toStrict $ mkMessage wl_shmID 0 messageBody
  sendManyWithFds sock [msg] [fileDescriptor]

  putStrLn $ printf " --> wl_shm@%i.create_pool: newID=%i fd=%s size=%i" wl_shmID newObjectID (show fileDescriptor :: Text) size
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

zwlrLayerShellV1GetLayerSurface :: Socket -> IORef ObjectTracker -> (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> ByteString -> Word32 -> IO Word32
zwlrLayerShellV1GetLayerSurface sock trackerRef updateFn layer namespace newObjectID = do
  tracker <- readIORef trackerRef
  let zwlr_layer_shell_v1ID = fromJust tracker.zwlr_layer_shell_v1ID
  let wl_surfaceID = fromJust tracker.wl_surfaceID

  let messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le wl_surfaceID
        putWord32le waylandNull
        putWord32le layer
        putWlString namespace
  sendAll sock $ mkMessage zwlr_layer_shell_v1ID 0 messageBody

  putStrLn $ printf " --> zwlr_layer_shell_v1@%i.get_layer_surface: newID=%i wl_surface=%i output=%i layer=%i namespace=%s" zwlr_layer_shell_v1ID newObjectID wl_surfaceID waylandNull layer (BSL.unpackChars namespace)
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

zwlrLayerSurfaceV1setAnchor :: Socket -> IORef ObjectTracker -> Word32 -> IO ()
zwlrLayerSurfaceV1setAnchor sock trackerRef anchor = do
  tracker <- readIORef trackerRef
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ putWord32le anchor
  sendAll sock $ mkMessage zwlr_layer_surface_v1ID 1 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.set_anchor: anchor=%i" zwlr_layer_surface_v1ID anchor

zwlrLayerSurfaceV1setSize :: Socket -> IORef ObjectTracker -> Word32 -> Word32 -> IO ()
zwlrLayerSurfaceV1setSize sock trackerRef width height = do
  tracker <- readIORef trackerRef
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ do
        putWord32le width
        putWord32le height
  sendAll sock $ mkMessage zwlr_layer_surface_v1ID 0 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.set_size: width=%i height=%i" zwlr_layer_surface_v1ID width height

zwlrLayerSurfaceV1ackConfigure :: Socket -> IORef ObjectTracker -> IO ()
zwlrLayerSurfaceV1ackConfigure sock trackerRef = do
  tracker <- readIORef trackerRef
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID
  let zwlr_layer_surface_v1Serial = fromJust tracker.zwlr_layer_surface_v1Serial

  let messageBody = runPut $ do putWord32le zwlr_layer_surface_v1Serial
  sendAll sock $ mkMessage zwlr_layer_surface_v1ID 6 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.ack_configure: serial=%i" zwlr_layer_surface_v1ID zwlr_layer_surface_v1Serial

zwlrLayerSurfaceV1setExclusiveZone :: Socket -> IORef ObjectTracker -> Int32 -> IO ()
zwlrLayerSurfaceV1setExclusiveZone sock trackerRef zone = do
  tracker <- readIORef trackerRef
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ do putInt32le zone
  sendAll sock $ mkMessage zwlr_layer_surface_v1ID 2 messageBody

  putStrLn $ printf " --> zwlr_layer_surface_v1@%i.set_exclusive_zone: zone=%i" zwlr_layer_surface_v1ID zone

wlShmPoolCreateBuffer :: Socket -> IORef ObjectTracker -> (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Word32
wlShmPoolCreateBuffer sock trackerRef updateFn offset width height stride format newObjectID = do
  tracker <- readIORef trackerRef
  let wl_shm_poolID = fromJust tracker.wl_shm_poolID

  let messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le offset
        putWord32le width
        putWord32le height
        putWord32le stride
        putWord32le format
  sendAll sock $ mkMessage wl_shm_poolID 0 messageBody

  putStrLn $ printf " --> wl_shm_pool@%i.create_buffer: newID=%i" wl_shm_poolID newObjectID
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

findInterface :: [(Header, EventGlobal)] -> ByteString -> Maybe EventGlobal
findInterface messages targetInterface =
  let target = targetInterface <> "\0"
   in Relude.find (\(_, e) -> target `isPrefixOf` e.interface) messages >>= Just . snd

bindToInterface :: [(Header, EventGlobal)] -> ByteString -> IORef Word32 -> (Word32 -> ByteString -> Word32 -> Word32 -> IO Word32) -> IO (Maybe Word32)
bindToInterface globals targetInterface counter wlRegistryBind' =
  case findInterface globals targetInterface of
    Nothing -> putStrLn ("ERROR: " <> BSL.unpackChars targetInterface <> " not found") >> pure Nothing
    Just e -> do
      objID <- nextID counter
      Just <$> wlRegistryBind' e.name targetInterface e.version objID

eventLoop :: Socket -> IORef ObjectTracker -> IO ()
eventLoop wl_display refTracker = do
  -- putStrLn "Waiting for events..."
  msg <- receiveSocketData wl_display
  tracker <- readIORef refTracker
  unless (BSL.null msg) $ do
    let events = runGet (parseEvents tracker) msg
    forM_ events $ \event -> do
      displayEvent event
      -- Handle events that need responses
      handleEventResponse wl_display refTracker event
  eventLoop wl_display refTracker

handleEventResponse :: Socket -> IORef ObjectTracker -> WaylandEvent -> IO ()
handleEventResponse wl_display refTracker (Event h e) = do
  whenJust (cast e) $ \(configure :: EventWlrLayerSurfaceConfigure) -> modifyIORef' refTracker (\t -> t{zwlr_layer_surface_v1Serial = Just configure.serial})
handleEventResponse _ _ _ = return ()

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
  counter <- newIORef 2 -- Start from 2 as ID 1 is always wl_display
  wl_display <- wlDisplayConnect
  wl_registry <- wlDisplayGetRegistry wl_display =<< nextID counter
  trackerRef <- newIORef $ ObjectTracker (Just wl_registry) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

  socketData <- receiveSocketData wl_display
  tracker <- readIORef trackerRef
  let initialEvents = runGet (parseEvents tracker) socketData
  mapM_ displayEvent initialEvents

  -- Extract globals from events
  let globals =
        [ (h, g)
        | ev <- initialEvents
        , Event h e <- [ev]
        , Just g <- [cast e :: Maybe EventGlobal]
        ]

  let colorChannels :: Word32 = 4
  let bufferWidth :: Word32 = 1920
  let bufferHeight :: Word32 = 25
  let stride :: Word32 = bufferWidth * colorChannels
  let colorFormat :: Word32 = 0 -- ARGB8888
  let sharedPoolSize :: Word32 = bufferWidth * bufferHeight * colorChannels
  let poolName :: String = "saybar-shared-pool"

  void
    . forkIO
    $ finally
      ( putStrLn "\n--- Starting event loop ---"
          >> eventLoop wl_display trackerRef
      )
      (close wl_display)

  threadDelay 100000

  -- Bind to the required interfaces
  putStrLn "\n--- Binding to interfaces ---"
  let wlRegistryBind' = wlRegistryBind wl_display wl_registry trackerRef
  _wl_shm <- bindToInterface globals "wl_shm" counter (wlRegistryBind' (\t objectID -> t{wl_shmID = objectID}))
  _wl_compositor <- bindToInterface globals "wl_compositor" counter (wlRegistryBind' (\t objectID -> t{wl_compositorID = objectID}))
  _zwlr_layer_shell_v1 <- bindToInterface globals "zwlr_layer_shell_v1" counter (wlRegistryBind' (\t objectID -> t{zwlr_layer_shell_v1ID = objectID}))

  _wl_surface <- wlCompositorCreateSurface wl_display trackerRef (\t objectID -> t{wl_surfaceID = objectID}) =<< nextID counter
  _zwlr_layer_surface_v1 <- zwlrLayerShellV1GetLayerSurface wl_display trackerRef (\t objectID -> t{zwlr_layer_surface_v1ID = objectID}) 2 "saybar" =<< nextID counter
  void $ zwlrLayerSurfaceV1setAnchor wl_display trackerRef 13 -- top left right anchors
  void $ zwlrLayerSurfaceV1setSize wl_display trackerRef 0 bufferHeight -- top left right anchors
  void $ zwlrLayerSurfaceV1setExclusiveZone wl_display trackerRef (fromIntegral bufferHeight)
  void $ wlSurfaceCommit wl_display trackerRef
  threadDelay 100000 -- Wait for response from commit
  void $ zwlrLayerSurfaceV1ackConfigure wl_display trackerRef

  font <- either (error . toText) pure =<< loadFontFile "CourierPrime-Regular.ttf"
  let bgColor = PixelRGBA8 0 0 0 255
      drawColor = PixelRGBA8 255 255 255 255
      img = renderDrawing (fromIntegral bufferWidth) (fromIntegral bufferHeight) bgColor $ do
        withTexture (uniformTexture drawColor) $ do
          printTextAt font (PointSize 12) (V2 20 15) "date: 2026-02-18 21:13. internet: connected. tray: steam and discord open. Data last updated at compile time with my keyboard"
  writePng "text_example.png" img

  void
    $ bracket
      (shmOpen poolName (ShmOpenFlags True True True True) 0600)
      (\_fd -> shmUnlink poolName)
      ( \fileDescriptor -> do
          setFdSize fileDescriptor (fromIntegral sharedPoolSize)
          _wl_shm_pool <- wlShmCreatePool wl_display trackerRef (\t objectID -> t{wl_shm_poolID = objectID}) fileDescriptor sharedPoolSize =<< nextID counter
          _wl_buffer <- wlShmPoolCreateBuffer wl_display trackerRef (\t objectID -> t{wl_bufferID = objectID}) 0 bufferWidth bufferHeight stride colorFormat =<< nextID counter

          file_handle <- fdToHandle fileDescriptor

          let swizzleRGBAtoBGRA :: Image PixelRGBA8 -> ByteString
              swizzleRGBAtoBGRA image =
                pack . go . VS.toList $ imageData image
                where
                  go [] = []
                  go (r : g : b : a : rest) = b : g : r : a : go rest
                  go _ = []

          let pixelData' = swizzleRGBAtoBGRA img
          let pixelData = runPut $ do replicateM_ (fromIntegral $ bufferWidth * bufferHeight) $ putWord32le 0x00FF0000
          hPut file_handle pixelData'

          void $ wlSurfaceAttach wl_display trackerRef
          void $ wlSurfaceCommit wl_display trackerRef
          threadDelay maxBound
      )
