module Requests (wlDisplay_getRegistry, wlShm_createPool, wlSurface_attach, zwlrLayerShellV1_getLayerSurface, zwlrLayerSurfaceV1_setAnchor, zwlrLayerSurfaceV1_setSize, zwlrLayerSurfaceV1_ackConfigure, zwlrLayerSurfaceV1_setExclusiveZone, wlShmPool_createBuffer, wlRegistry_bind, wlCompositor_createSurface, wlSurface_commit) where

import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Internal qualified as BSL
import Network.Socket
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)
import Relude.Unsafe (fromJust)
import System.Posix.Types
import Text.Printf
import Types
import Utils

{- HLINT ignore "Use camelCase" -}

wlDisplay_getRegistry :: Socket -> IORef Word32 -> IO Word32
wlDisplay_getRegistry sock counter = do
  registryID <- nextID' counter
  let messageBody = runPut $ putWord32le registryID
  liftIO $ sendAll sock $ mkMessage wlDisplayID 1 messageBody
  strReq ("wl_display", wlDisplayID, "get_registry") $ printf "wl_registry=%i" registryID
  pure registryID

wlShm_createPool :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Fd -> Word32 -> Wayland ()
wlShm_createPool updateFn fileDescriptor size = do
  env <- ask
  newObjectID <- nextID env.counter
  let wl_shmID = env.wl_shmID
      messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le size
  let msg = BS.toStrict $ mkMessage wl_shmID 0 messageBody
  liftIO $ sendManyWithFds env.socket [msg] [fileDescriptor]

  let sender = ("wl_shm", wl_shmID, "create_pool")
  liftIO . strReq sender $ printf "newID=%i fd=%s size=%i" newObjectID (show @Text fileDescriptor) size

  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

wlSurface_attach :: Wayland ()
wlSurface_attach = do
  env <- ask
  tracker <- readIORef env.tracker
  let wl_surfaceID = fromJust tracker.wl_surfaceID
      wl_bufferID = fromJust tracker.wl_bufferID

  let messageBody = runPut $ do
        putWord32le wl_bufferID
        -- x y arguments have to be set to 0
        putInt32le 0
        putInt32le 0
  liftIO . sendAll env.socket $ mkMessage wl_surfaceID 1 messageBody

  let sender = ("wl_surface", wl_surfaceID, "attach")
  liftIO . strReq sender $ printf "bufferId=%i x=%i y=%i" wl_bufferID (0 :: Int32) (0 :: Int32)

zwlrLayerShellV1_getLayerSurface :: (ObjectTracker -> Maybe Word32 -> ObjectTracker) -> Word32 -> ByteString -> Wayland ()
zwlrLayerShellV1_getLayerSurface updateFn layer namespace = do
  env <- ask
  tracker <- readIORef env.tracker
  newObjectID <- nextID env.counter
  let zwlr_layer_shell_v1ID = env.zwlr_layer_shell_v1ID
      wl_surfaceID = fromJust tracker.wl_surfaceID

  let messageBody = runPut $ do
        putWord32le newObjectID
        putWord32le wl_surfaceID
        putWord32le waylandNull
        putWord32le layer
        putWlString namespace
  liftIO . sendAll env.socket $ mkMessage zwlr_layer_shell_v1ID 0 messageBody

  let sender = ("zwlr_layer_shell_v1", zwlr_layer_shell_v1ID, "get_layer_surface")
  liftIO
    . strReq sender
    $ printf "newID=%i wl_surface=%i output=%i layer=%i namespace=%s" newObjectID wl_surfaceID waylandNull layer (BSL.unpackChars namespace)

  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

zwlrLayerSurfaceV1_setAnchor :: Word32 -> Wayland ()
zwlrLayerSurfaceV1_setAnchor anchor = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ putWord32le anchor
  liftIO $ sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 1 messageBody

  let sender = ("zwlr_layer_surface_v1", zwlr_layer_surface_v1ID, "set_anchor")
  liftIO . strReq sender $ printf "anchor=%i" anchor

zwlrLayerSurfaceV1_setSize :: Word32 -> Word32 -> Wayland ()
zwlrLayerSurfaceV1_setSize width height = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ do
        putWord32le width
        putWord32le height
  liftIO . sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 0 messageBody

  let sender = ("zwlr_layer_surface_v1", zwlr_layer_surface_v1ID, "set_size")
  liftIO . strReq sender $ printf "width=%i height=%i" width height

zwlrLayerSurfaceV1_ackConfigure :: Wayland ()
zwlrLayerSurfaceV1_ackConfigure = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID
      zwlr_layer_surface_v1Serial = fromJust tracker.zwlr_layer_surface_v1Serial

  let messageBody = runPut $ do putWord32le zwlr_layer_surface_v1Serial
  liftIO . sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 6 messageBody

  let sender = ("zwlr_layer_surface_v1", zwlr_layer_surface_v1ID, "ack_configure")
  liftIO . strReq sender $ printf "serial=%i" zwlr_layer_surface_v1Serial

zwlrLayerSurfaceV1_setExclusiveZone :: Int32 -> Wayland ()
zwlrLayerSurfaceV1_setExclusiveZone zone = do
  env <- ask
  tracker <- readIORef env.tracker
  let zwlr_layer_surface_v1ID = fromJust tracker.zwlr_layer_surface_v1ID

  let messageBody = runPut $ do putInt32le zone
  liftIO $ sendAll env.socket $ mkMessage zwlr_layer_surface_v1ID 2 messageBody

  let sender = ("zwlr_layer_surface_v1", zwlr_layer_surface_v1ID, "set_exclusive_zone")
  liftIO . strReq sender $ printf "zone=%i" zone

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

  let sender = ("wl_shm_pool", wl_shm_poolID, "create_buffer")
  liftIO . strReq sender $ printf "newID=%i" newObjectID
  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

wlRegistry_bind :: Socket -> Word32 -> Word32 -> ByteString -> Word32 -> Word32 -> IO Word32
wlRegistry_bind sock registryID globalName interfaceName interfaceVersion newObjectID = do
  let messageBody = runPut $ do
        putWord32le globalName
        putWlString interfaceName
        putWord32le interfaceVersion
        putWord32le newObjectID
  liftIO $ sendAll sock $ mkMessage registryID 0 messageBody

  let sender = ("wl_registry", registryID, "bind")
  strReq sender
    $ printf
      "name=%i interface=\"%s\" version=%i id=%i"
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

  let sender = ("wl_compositor", env.wl_compositorID, "create_surface")
  liftIO . strReq sender $ printf "newID=%i" newObjectID
  modifyIORef' env.tracker $ \t -> updateFn t (Just newObjectID)

wlSurface_commit :: Wayland ()
wlSurface_commit = do
  env <- ask
  wl_surfaceID <- fromJust . (.wl_surfaceID) <$> readIORef env.tracker

  let messageBody = runPut mempty
  liftIO . sendAll env.socket $ mkMessage wl_surfaceID 6 messageBody

  let sender = ("wl_surface", wl_surfaceID, "commit")
  liftIO $ strReq sender "commit request"
