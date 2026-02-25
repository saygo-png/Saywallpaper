{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Codec.Picture (PixelRGBA8 (..))
import Codec.Picture.Types (Image)
import Config
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Internal qualified as BSL
import Data.Maybe (fromJust)
import Data.Typeable
import Events
import GHC.IO.Handle
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (loadFontFile)
import Headers
import Network.Socket
import Relude hiding (ByteString, get, isPrefixOf, put)
import Requests
import System.Environment (getEnv)
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem
import System.Process.Typed
import Types
import Utils

wlDisplayConnect :: IO Socket
wlDisplayConnect = do
  xdg_runtime_dir <- getEnv "XDG_RUNTIME_DIR"
  wayland_display <- getEnv "WAYLAND_DISPLAY"
  let path = xdg_runtime_dir <> "/" <> wayland_display
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  return sock

parseEvent :: Word32 -> Maybe Word32 -> ObjectTracker -> Get WaylandEvent
parseEvent registryID wl_shmID tracker = do
  header <- get
  let matchEvent' = matchEvent header
      maybeMatchEvent' = maybeMatchEvent header
      bodySize = fromIntegral header.size - 8
      ev :: (Binary a, WaylandEventType a, Typeable a) => Get a -> Get WaylandEvent
      ev = fmap (Event header)
  if
    | matchEvent' wlDisplayID 0 -> ev (get @EventDisplayError)
    | matchEvent' wlDisplayID 1 -> ev (get @EventDisplayDeleteId)
    | matchEvent' registryID 0 -> ev (get @EventGlobal)
    | matchEvent' registryID 1 -> skip bodySize $> EvUnknown header
    | maybeMatchEvent' wl_shmID 0 -> ev (get @EventShmFormat)
    | maybeMatchEvent' tracker.zwlr_layer_surface_v1ID 0 -> ev (get @EventWlrLayerSurfaceConfigure)
    | matchBufferEvent header tracker.wl_buffer_A 0 -> pure $ EvEmpty header EventBufferRelease
    | matchBufferEvent header tracker.wl_buffer_B 0 -> pure $ EvEmpty header EventBufferRelease
    | otherwise -> skip bodySize $> EvUnknown header
  where
    maybeMatchEvent :: Header -> Maybe Word32 -> Word16 -> Bool
    maybeMatchEvent header (Just oid) opcode = matchEvent header oid opcode
    maybeMatchEvent _ Nothing _ = False

    matchBufferEvent :: Header -> Maybe Buffer -> Word16 -> Bool
    matchBufferEvent header (Just buffer) opcode = matchEvent header buffer.id opcode
    matchBufferEvent _ Nothing _ = False

    matchEvent :: Header -> Word32 -> Word16 -> Bool
    matchEvent header oid opcode = oid == header.objectID && header.opCode == opcode

parseEvents :: Word32 -> Maybe Word32 -> ObjectTracker -> Get [WaylandEvent]
parseEvents registryID wl_shmID tracker = do
  isEmpty >>= \case
    True -> return []
    False -> (:) <$> parseEvent registryID wl_shmID tracker <*> parseEvents registryID wl_shmID tracker

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
handleEventResponse (Event _ e) = do
  tracker <- readIORef =<< asks (.tracker)
  whenJust (cast e) $ \(ev :: EventWlrLayerSurfaceConfigure) ->
    atomically $ putTMVar tracker.zwlr_layer_surface_v1Serial ev.serial
handleEventResponse _ = return ()

getBarState :: IO BarState
getBarState = do
  (dateOut, dateErr) <- readProcess_ "date"
  pure . BarState $ decodeUtf8 dateOut

renderBarState :: BarState -> Image PixelRGBA8
renderBarState = undefined

main :: IO ()
main = runReaderT program =<< waylandSetup

waylandSetup :: IO WaylandEnv
waylandSetup = do
  sock <- wlDisplayConnect
  counter <- newIORef 2 -- start from 2 because wl_display is always 1
  registry <- wlDisplay_getRegistry sock counter
  socketData <- receiveSocketData sock
  tracker <- newIORef . ObjectTracker Nothing Nothing Nothing Nothing Nothing =<< newEmptyTMVarIO

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

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT eventLoop env)
      (close env.socket)

  wlCompositor_createSurface $ \t objectID -> t{wl_surfaceID = objectID}
  zwlrLayerShellV1_getLayerSurface (\t objectID -> t{zwlr_layer_surface_v1ID = objectID}) 2 "saybar"
  zwlrLayerSurfaceV1_setAnchor 13 -- top left right anchors
  zwlrLayerSurfaceV1_setSize 0 bufferHeight
  zwlrLayerSurfaceV1_setExclusiveZone (fromIntegral bufferHeight)
  wlSurface_commit
  zwlrLayerSurfaceV1_ackConfigure

  font <- either (error . toText) pure =<< liftIO (loadFontFile "CourierPrime-Regular.ttf")
  let bgColor = PixelRGBA8 0 0 0 0
      drawColor = PixelRGBA8 213 196 161 255 -- #d5c4a1
      img = renderDrawing (fromIntegral bufferWidth) (fromIntegral bufferHeight) bgColor $ do
        withTexture (uniformTexture drawColor) $ do
          printTextAt font (PointSize 12) (V2 20 15) "date: 2026-02-18 21:13. internet: connected. tray: steam and discord open. Data last updated at compile time with my keyboard"
      img2 = renderDrawing (fromIntegral bufferWidth) (fromIntegral bufferHeight) bgColor $ do
        withTexture (uniformTexture drawColor) $ do
          printTextAt font (PointSize 12) (V2 20 15) "foobar"

      img3 = renderDrawing (fromIntegral bufferWidth) (fromIntegral bufferHeight) bgColor $ do
        withTexture (uniformTexture drawColor) $ do
          printTextAt font (PointSize 12) (V2 20 15) "foobar 3"

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      removeSharedMemoryObject _ = shmUnlink poolName
      useSharedMemoryObject fileDescriptor =
        flip runReaderT env $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          let poolSize = 2 * frameSize -- 2x for double buffering
          liftIO . setFdSize fileDescriptor $ fromIntegral poolSize
          wlShm_createPool (\t objectID -> t{wl_shm_poolID = objectID}) poolSize fileDescriptor
          wlShmPool_createBuffer (\t buffer -> t{wl_buffer_A = buffer}) 0
          wlShmPool_createBuffer (\t buffer -> t{wl_buffer_B = buffer}) frameSize

          file_handle <- liftIO $ fdToHandle fileDescriptor

          putImage file_handle img BufferA
          liftIO $ threadDelay 1000000

          putImage file_handle img2 BufferB
          liftIO $ threadDelay 1000000

          putImage file_handle img3 BufferA
          liftIO $ threadDelay maxBound

  liftIO . void $ bracket makeSharedMemoryObject removeSharedMemoryObject useSharedMemoryObject

putImage :: Handle -> Image PixelRGBA8 -> WhichBuffer -> Wayland ()
putImage fileHandle image whichBuffer = do
  tracker <- readIORef =<< asks (.tracker)
  let buffer = fromJust $ case whichBuffer of
        BufferA -> tracker.wl_buffer_A
        BufferB -> tracker.wl_buffer_B
  liftIO . hSeek fileHandle AbsoluteSeek $ fromIntegral buffer.offset
  liftIO . hPut fileHandle $ swizzleRGBAtoBGRA image
  wlSurface_damageBuffer 0 0 bufferWidth bufferHeight
  wlSurface_attach buffer.id
  wlSurface_commit
