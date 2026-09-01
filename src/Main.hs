{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Config
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (newTQueue, retry, stateTVar)
import Control.Exception
import Data.Bimap qualified as BM
import Data.ByteString.Lazy
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified as Set
import Network.Socket hiding (openSocket)
import Relude hiding (ByteString, get, isPrefixOf, put)
import Relude.Unsafe qualified as Unsafe
import Saywayland
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO hiding (dup)
import System.Posix.SharedMem

interfaceTable :: InterfaceClientTable
interfaceTable = waylandInterfaceClientTable <> wlr_layer_shell_unstable_v1InterfaceClientTable

versionTable :: VersionTable
versionTable = waylandVersionTable <> wlr_layer_shell_unstable_v1VersionTable

data LayerPhase
  = Idle
  | AwaitingConfigure Zwlr_layer_surface_v1
  | LayerConfigured Zwlr_layer_surface_v1

phaseSurface :: LayerPhase -> Maybe Zwlr_layer_surface_v1
phaseSurface Idle = Nothing
phaseSurface (AwaitingConfigure s) = Just s
phaseSurface (LayerConfigured s) = Just s

-- | Shared state between threads
data SurfaceStates = SurfaceStates
  { outputNames :: Set Word32
  , layerPhase :: LayerPhase
  , pendingBuffer :: Maybe Wl_buffer
  , bufferAttached :: Bool
  }

hasAnyOutput :: SurfaceStates -> Bool
hasAnyOutput = not . Set.null . (.outputNames)

newtype RunRequestS = RunRequestS (forall a. (Interface' a Client) => a -> Request a -> Wayland Client ())

-- Get a function which runs a request with an mvar
getRunRequestS :: IO RunRequestS
getRunRequestS = do
  lock <- newMVar ()
  pure $ RunRequestS $ \i req -> do
    clientEnv <- ask
    liftIO $ withMVar lock $ \_ -> usingReaderT clientEnv (runRequest i req)

main :: IO ()
main = do
  wallpaperPath <-
    getArgs >>= \case
      "-i" : p : _ -> pure p
      _ -> putStrLn "Provide a path to the image as an argument using \"-i <path>\"" >> exitFailure
  runReaderT (program wallpaperPath) =<< do
    let display :: Interface Client = Interface $ Wl_display $ TObjectID wlDisplayID
    getSocketPath openSocket >>= \case
      Just path -> do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock $ SockAddrUnix path
        counter <- newIORef $ coerce wlDisplayID
        globals <- newIORef BM.empty
        objects <- newIORef $ fromList [(wlDisplayID, display)]
        handlers <- newIORef mempty
        interfaceTable' <- newIORef $ fromList interfaceTable
        versionTable' <- newIORef $ fromList versionTable
        fdqueue <- atomically newTQueue
        pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers fdqueue
      Nothing -> error "couldn't find `$WAYLAND_DISPLAY` nor any open socket."

program :: FilePath -> Wayland Client ()
program wallpaperPath = do
  ClientEnv env <- ask
  running <- newEmptyMVar
  stateVar <- liftIO . newTVarIO $ SurfaceStates{outputNames = Set.empty, layerPhase = Idle, pendingBuffer = Nothing, bufferAttached = False}

  RunRequestS runRequestS <- liftIO getRunRequestS

  -- Runs an STM transition against the shared state, then performs
  -- whatever side effect that transition decided on.
  let transact :: State SurfaceStates (Wayland Client ()) -> Wayland Client ()
      transact action = join . liftIO . atomically $ stateTVar stateVar (runState action)

  registryId <- TObjectID <$> newObjectId
  display <- Unsafe.fromJust <$> getInterface' @Wl_display 1
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

  -- wl_output is not bound as its not implemented in saywayland.

  let onEvent :: (WaylandEvent e) => (e -> Wayland Client ()) -> Wayland Client ()
      onEvent x = modifyIORef env.eventHandlers $ (:) $ EventHandler (const x)

  onEvent $ \case
    Event_wl_registry_global name "wl_output\NUL" _ -> transact $ do
      modify $ \s -> s{outputNames = Set.insert name s.outputNames}
      pure pass
    Event_wl_registry_global_remove name -> transact $ do
      modify $ \s -> s{outputNames = Set.delete name s.outputNames}
      pure pass
    _ -> pass

  onEvent $ \case
    Event_zwlr_layer_surface_v1_configure serial _ _ -> transact $ do
      gets (.layerPhase) >>= \case
        AwaitingConfigure surf -> do
          modify' $ \s -> s{layerPhase = LayerConfigured surf}
          pure $ runRequest surf (Request_zwlr_layer_surface_v1_ack_configure serial)
        _ ->
          pure pass
    Event_zwlr_layer_surface_v1_closed -> transact $ do
      oldPhase <- gets (.layerPhase)
      modify' $ \s -> s{layerPhase = Idle, bufferAttached = False}
      pure . forM_ (phaseSurface oldPhase) $ \surf ->
        runRequest surf Request_zwlr_layer_surface_v1_destroy

  initialGlobals <- liftIO $ BM.toList <$> readIORef env.globals
  liftIO $ putStrLn $ "[startup] globals entries seen: " <> show (Relude.length initialGlobals)
  liftIO $ putStrLn $ "[startup] globals contents: " <> show initialGlobals
  let initialOutputNames = Set.fromList [name | (interfaceName, name) <- initialGlobals, interfaceName == "wl_output"]
  liftIO $ putStrLn $ "[startup] initialOutputNames: " <> show (Set.toList initialOutputNames)
  liftIO . atomically $ modifyTVar' stateVar $ \s -> s{outputNames = s.outputNames <> initialOutputNames}

  let reconcileStep :: Wayland Client ()
      reconcileStep = do
        action <- liftIO . atomically $ do
          s <- readTVar stateVar
          case (hasAnyOutput s, s.layerPhase, s.pendingBuffer, s.bufferAttached) of
            (True, Idle, _, _) -> pure createLayerSurface
            (_, LayerConfigured _, Just buf, False) -> do
              writeTVar stateVar s{bufferAttached = True}
              pure (attachBuffer buf)
            _ -> retry
        liftIO $ putStrLn "[reconciler] woke up with an action, running it"
        action
        where
          createLayerSurface :: Wayland Client ()
          createLayerSurface = do
            layerSurfaceId <- TObjectID <$> newObjectId
            runRequestS zwlr_layer_shell_V1 $ Request_zwlr_layer_shell_v1_get_layer_surface layerSurfaceId wlSurfaceId 0 Enum_zwlr_layer_shell_v1_layer_background "wallpaper"
            zwlrLayerSurface <- Unsafe.fromJust <$> getInterface layerSurfaceId

            runRequest zwlrLayerSurface $ Request_zwlr_layer_surface_v1_set_size (fromIntegral bufferWidth) (fromIntegral bufferHeight)
            runRequest zwlrLayerSurface $ Request_zwlr_layer_surface_v1_set_exclusive_zone $ -1
            runRequest surface' Request_wl_surface_commit
            liftIO . atomically $ modifyTVar' stateVar $ \s -> s{layerPhase = AwaitingConfigure zwlrLayerSurface}

          attachBuffer :: Wl_buffer -> Wayland Client ()
          attachBuffer wlBuffer = do
            runRequest surface' $ Request_wl_surface_attach wlBuffer.wlid 0 0
            runRequest surface' Request_wl_surface_commit

  liftIO $ putStrLn "[startup] forking reconciler"
  liftIO . void . forkIO . runReaderT (putStrLn "[reconciler] thread started" >> forever reconcileStep) $ ClientEnv env

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      removeSharedMemoryObject _ = shmUnlink poolName
      useSharedMemoryObject fileDescriptor =
        usingReaderT (ClientEnv env) $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          let poolSize = frameSize
          liftIO . setFdSize fileDescriptor $ fromIntegral poolSize

          wlShmPoolId <- TObjectID <$> newObjectId
          runRequestS wl_shm $ Request_wl_shm_create_pool wlShmPoolId fileDescriptor frameSize
          wl_shm_pool <- Unsafe.fromJust <$> getInterface wlShmPoolId

          wlBufferId <- TObjectID <$> newObjectId
          runRequestS wl_shm_pool $ Request_wl_shm_pool_create_buffer wlBufferId 0 bufferWidth bufferHeight (bufferWidth * colorChannels) colorFormat
          wlBuffer <- getInterface wlBufferId

          fileHandle <- liftIO $ fdToHandle fileDescriptor
          liftIO $ hPut fileHandle =<< BSL.readFile wallpaperPath

          liftIO . atomically $ modifyTVar' stateVar $ \s -> s{pendingBuffer = wlBuffer}

          -- Wait for exit
          liftIO $ takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject removeSharedMemoryObject useSharedMemoryObject
