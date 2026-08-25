{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Config
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (isEmptyMVar)
import Control.Concurrent.STM (newTQueue)
import Control.Exception
import Data.Bimap qualified as BM
import Data.ByteString.Lazy
import Data.ByteString.Lazy qualified as BSL
import Network.Socket hiding (openSocket)
import Relude hiding (ByteString, get, isPrefixOf, put)
import Relude.Unsafe qualified as Unsafe
import Saywayland
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem

interfaceTable :: InterfaceClientTable
interfaceTable = waylandInterfaceClientTable <> wlr_layer_shell_unstable_v1InterfaceClientTable

versionTable :: VersionTable
versionTable = waylandVersionTable <> wlr_layer_shell_unstable_v1VersionTable

main :: IO ()
main = do
  wallpaperPath <-
    getArgs >>= \case
      "-i" : p : _ -> pure p
      _ -> putStrLn "Provide a path to the image as an argument using \"-i <path>\"" >> exitFailure
  runReaderT (program wallpaperPath) =<< waylandSetup
  where
    waylandSetup = do
      getSocketPath openSocket >>= \case
        Just path -> do
          sock <- socket AF_UNIX Stream defaultProtocol
          connect sock $ SockAddrUnix path
          counter <- newIORef $ coerce wlDisplayID
          globals <- newIORef BM.empty
          objects <- newIORef mempty
          handlers <- newIORef mempty
          interfaceTable' <- newIORef $ fromList interfaceTable
          versionTable' <- newIORef $ fromList versionTable
          fdqueue <- atomically newTQueue
          pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers fdqueue
        Nothing -> error "couldn't find `$WAYLAND_DISPLAY` nor any open socket."

program :: FilePath -> Wayland Client ()
program wallpaperPath = do
  ClientEnv env <- ask
  serial :: TMVar Word32 <- newEmptyTMVarIO
  running :: MVar () <- newEmptyMVar
  let display :: Wl_display = Wl_display $ TObjectID wlDisplayID

  registryId <- TObjectID <$> newObjectId
  runRequest display $ Request_wl_display_get_registry registryId
  registry <- Unsafe.fromJust <$> getInterface registryId

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())

  putStrLn "Binding to required interfaces..."

  wlShmId <- TObjectID . Unsafe.fromJust <$> bindToInterface registry "wl_shm"
  wl_shm :: Wl_shm <- Unsafe.fromJust <$> getInterface wlShmId

  wlCompositorId <- TObjectID . Unsafe.fromJust <$> bindToInterface registry "wl_compositor"
  wl_compositor :: Wl_compositor <- Unsafe.fromJust <$> getInterface wlCompositorId

  zwlrLayerShellV1Id <- TObjectID . Unsafe.fromJust <$> bindToInterface registry "zwlr_layer_shell_v1"
  zwlr_layer_shell_V1 :: Zwlr_layer_shell_v1 <- Unsafe.fromJust <$> getInterface zwlrLayerShellV1Id

  wlSurfaceId <- TObjectID <$> newObjectId
  runRequest wl_compositor $ Request_wl_compositor_create_surface wlSurfaceId
  surface' <- Unsafe.fromJust <$> getInterface wlSurfaceId

  modifyIORef env.eventHandlers $ (:) $ EventHandler $ \_ -> \case
    (Event_zwlr_layer_surface_v1_configure receivedSerial _ _) -> do
      atomically $ putTMVar serial receivedSerial
    Event_zwlr_layer_surface_v1_closed -> putStrLn "Output died. TODO: Remake surface on new output"

  let layerSurfaceActions wlOutput = do
        layerSurfaceId <- TObjectID <$> newObjectId
        runRequest zwlr_layer_shell_V1 $ Request_zwlr_layer_shell_v1_get_layer_surface layerSurfaceId wlSurfaceId wlOutput Enum_zwlr_layer_shell_v1_layer_background "wallpaper"
        zwlrLayerSurface <- Unsafe.fromJust <$> getInterface layerSurfaceId

        runRequest zwlrLayerSurface $ Request_zwlr_layer_surface_v1_set_size (fromIntegral bufferWidth) (fromIntegral bufferHeight)
        runRequest zwlrLayerSurface $ Request_zwlr_layer_surface_v1_set_exclusive_zone $ -1

        runRequest surface' Request_wl_surface_commit
        atomically (takeTMVar serial) >>= runRequest zwlrLayerSurface . Request_zwlr_layer_surface_v1_ack_configure

  layerSurfaceActions 0

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      removeSharedMemoryObject _ = shmUnlink poolName
      useSharedMemoryObject fileDescriptor =
        usingReaderT (ClientEnv env) $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          let poolSize = frameSize
          liftIO . setFdSize fileDescriptor $ fromIntegral poolSize

          wlShmPoolId <- TObjectID <$> newObjectId
          runRequest wl_shm $ Request_wl_shm_create_pool wlShmPoolId fileDescriptor frameSize
          wl_shm_pool <- Unsafe.fromJust <$> getInterface wlShmPoolId

          wlBufferId <- TObjectID <$> newObjectId
          runRequest wl_shm_pool $ Request_wl_shm_pool_create_buffer wlBufferId 0 bufferWidth bufferHeight (bufferWidth * colorChannels) colorFormat

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle =<< BSL.readFile wallpaperPath
          runRequest surface' $ Request_wl_surface_attach wlBufferId 0 0
          runRequest surface' Request_wl_surface_commit

          -- Wait for exit
          takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject removeSharedMemoryObject useSharedMemoryObject
