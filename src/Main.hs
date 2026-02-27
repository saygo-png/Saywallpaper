{- HLINT ignore "Use camelCase" -}

module Main (main) where

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
import Headers
import Network.Socket
import Relude hiding (ByteString, get, isPrefixOf, put)
import Requests
import System.Environment (getEnv)
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem
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
    | otherwise -> skip bodySize $> EvUnknown header
  where
    maybeMatchEvent :: Header -> Maybe Word32 -> Word16 -> Bool
    maybeMatchEvent header (Just oid) opcode = matchEvent header oid opcode
    maybeMatchEvent _ Nothing _ = False

    matchBufferEvent :: Header -> Maybe Word32 -> Word16 -> Bool
    matchBufferEvent header (Just bufferID) opcode = matchEvent header bufferID opcode
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

main :: IO ()
main = do
  wallpaperPath <-
    getArgs >>= \case
      "-i" : p : _ -> pure p
      _ -> putStrLn "Provide a path to the image as an argument using \"-i <path>\"" >> exitFailure
  runReaderT (program wallpaperPath) =<< waylandSetup

waylandSetup :: IO WaylandEnv
waylandSetup = do
  sock <- wlDisplayConnect
  counter <- newIORef 2 -- start from 2 because wl_display is always 1
  registry <- wlDisplay_getRegistry sock counter
  socketData <- receiveSocketData sock
  tracker <- newIORef . ObjectTracker Nothing Nothing Nothing Nothing =<< newEmptyTMVarIO

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

program :: FilePath -> Wayland ()
program wallpaperPath = do
  env <- ask

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT eventLoop env)
      (close env.socket)

  wlCompositor_createSurface $ \t objectID -> t{wl_surfaceID = objectID}
  zwlrLayerShellV1_getLayerSurface (\t objectID -> t{zwlr_layer_surface_v1ID = objectID}) 0 "wallpaper"
  zwlrLayerSurfaceV1_setSize bufferWidth bufferHeight
  zwlrLayerSurfaceV1_setExclusiveZone (-1)
  wlSurface_commit
  zwlrLayerSurfaceV1_ackConfigure

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      removeSharedMemoryObject _ = shmUnlink poolName
      useSharedMemoryObject fileDescriptor =
        flip runReaderT env $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          let poolSize = frameSize
          liftIO . setFdSize fileDescriptor $ fromIntegral poolSize
          wlShm_createPool (\t objectID -> t{wl_shm_poolID = objectID}) poolSize fileDescriptor
          wlShmPool_createBuffer (\t buffer -> t{wl_buffer_A = buffer}) 0

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          img <- liftIO $ BSL.readFile wallpaperPath
          t <- readIORef env.tracker
          let bufferID = fromJust t.wl_buffer_A
          liftIO $ hPut fileHandle img
          wlSurface_attach bufferID
          wlSurface_commit
          liftIO $ threadDelay maxBound

  liftIO . void $ bracket makeSharedMemoryObject removeSharedMemoryObject useSharedMemoryObject
