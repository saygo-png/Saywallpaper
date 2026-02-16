{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Data.Binary hiding (get, put)
import Data.Binary qualified as Bin
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Internal qualified as BS
import Data.Int
import Data.Maybe (fromJust)
import Data.Typeable
import Events
import Headers
import Network.Socket
import Network.Socket.ByteString.Lazy
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)
import System.Environment (getEnv)
import System.Posix.IO
import System.Posix.SharedMem
import Text.Printf
import Utils

headerSize :: Int64
headerSize = 8

nextID :: IORef LeWord32 -> IO LeWord32
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
      | matchEvent' tracker.xdg_wm_baseID 0 -> ev (Bin.get @EventXdgWmBasePing)
      | matchEvent' tracker.xdg_surfaceID 0 -> ev (Bin.get @EventXdgSurfaceConfigure)
      | matchEvent' tracker.xdg_toplevelID 0 -> ev (Bin.get @EventXdgToplevelConfigure)
      | matchEvent' tracker.xdg_toplevelID 1 -> skip bodySize $> Event header EventXdgToplevelClose
      | matchEvent' tracker.xdg_toplevelID 2 -> ev (Bin.get @EventXdgToplevelConfigureBounds)
      | matchEvent' tracker.xdg_toplevelID 3 -> ev (Bin.get @EventXdgToplevelWmCapabilities)
      | otherwise -> skip bodySize $> EvUnknown header
  where
    matchEvent :: Header -> Maybe LeWord32 -> Word16 -> Bool
    matchEvent header (Just oid) opcode = coerce oid == header.objectID && header.opCode == opcode
    matchEvent _ Nothing _ = False

parseEvents :: ObjectTracker -> Get [WaylandEvent]
parseEvents tracker = do
  isEmpty >>= \case
    True -> return []
    False -> (:) <$> parseEvent tracker <*> parseEvents tracker

wlDisplayGetRegistry :: Socket -> LeWord32 -> IO LeWord32
wlDisplayGetRegistry wl_display newObjectID = do
  let wl_header = encode (Header 1 1 12, newObjectID)
  sendAll wl_display wl_header
  putStrLn "  --> wl_display@1.get_registry: wl_registry=2"
  return newObjectID

xdgWmBasePong :: Socket -> Word32 -> Word32 -> IO ()
xdgWmBasePong sock xdgWmBaseId serial = do
  let messageBody = runPut $ putWord32le serial
  sendAll sock $ mkMessage xdgWmBaseId 0 messageBody
  putStrLn $ printf "-> xdg_wm_base@%i.pong: serial=%i" xdgWmBaseId serial

wlRegistryBind :: Socket -> LeWord32 -> IORef ObjectTracker -> (ObjectTracker -> Maybe LeWord32 -> ObjectTracker) -> Word32 -> ByteString -> Word32 -> LeWord32 -> IO LeWord32
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
        putWord32le (coerce newObjectID) -- new_id
  sendAll sock $ mkMessage (coerce registryID) 0 messageBody

  putStrLn
    $ printf
      " --> wl_registry@%i.bind: name=%i interface=\"%s\" version=%i id=%i"
      registryID
      globalName
      (BS.unpackChars interfaceName)
      interfaceVersion
      newObjectID
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

wlCompositorCreateSurface :: Socket -> IORef ObjectTracker -> (ObjectTracker -> Maybe LeWord32 -> ObjectTracker) -> LeWord32 -> IO LeWord32
wlCompositorCreateSurface sock trackerRef updateFn newObjectID = do
  tracker <- readIORef trackerRef
  let wl_compositorID = fromJust tracker.wl_compositorID

  let messageBody = runPut $ putWord32le (coerce newObjectID)
  sendAll sock $ mkMessage (coerce wl_compositorID) 0 messageBody

  putStrLn $ printf " --> wl_compositor@%i.create_surface: newID=%i" wl_compositorID newObjectID
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

wlSurfaceCommit :: Socket -> IORef ObjectTracker -> IO ()
wlSurfaceCommit sock trackerRef = do
  tracker <- readIORef trackerRef
  let wl_surfaceID = fromJust tracker.wl_surfaceID

  let messageBody = runPut mempty
  sendAll sock $ mkMessage (coerce wl_surfaceID) 6 messageBody

  putStrLn $ printf " --> wl_surface@%i.commit: commit request" wl_surfaceID

xdgWmBaseCreateSurface :: Socket -> IORef ObjectTracker -> (ObjectTracker -> Maybe LeWord32 -> ObjectTracker) -> LeWord32 -> IO LeWord32
xdgWmBaseCreateSurface sock trackerRef updateFn newObjectID = do
  tracker <- readIORef trackerRef
  let xdg_wm_baseID = coerce $ fromJust tracker.xdg_wm_baseID
  let wl_surfaceID = coerce $ fromJust tracker.wl_surfaceID

  let messageBody = runPut $ do
        putWord32le (coerce newObjectID)
        putWord32le wl_surfaceID
  sendAll sock $ mkMessage xdg_wm_baseID 2 messageBody

  putStrLn $ printf " --> xdg_wm_base@%i.create_surface: newID=%i wl_surface=%i" xdg_wm_baseID newObjectID wl_surfaceID
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

xdgSurfaceGetTopLevel :: Socket -> IORef ObjectTracker -> (ObjectTracker -> Maybe LeWord32 -> ObjectTracker) -> LeWord32 -> IO LeWord32
xdgSurfaceGetTopLevel sock trackerRef updateFn newObjectID = do
  tracker <- readIORef trackerRef
  let xdg_surfaceID = coerce $ fromJust tracker.xdg_surfaceID

  let messageBody = runPut $ do
        putWord32le (coerce newObjectID)
  sendAll sock $ mkMessage xdg_surfaceID 1 messageBody

  putStrLn $ printf " --> xdg_surface@%i.get_toplevel: newID=%i" xdg_surfaceID newObjectID
  modifyIORef' trackerRef $ \t -> updateFn t (Just newObjectID)
  pure newObjectID

findInterface :: [(Header, EventGlobal)] -> ByteString -> Maybe EventGlobal
findInterface messages targetInterface =
  let target = targetInterface <> "\0"
   in Relude.find (\(_, e) -> target `isPrefixOf` e.interface) messages >>= Just . snd

bindToInterface :: [(Header, EventGlobal)] -> ByteString -> IORef LeWord32 -> (Word32 -> ByteString -> Word32 -> LeWord32 -> IO LeWord32) -> IO (Maybe LeWord32)
bindToInterface globals targetInterface counter wlRegistryBind' =
  case findInterface globals targetInterface of
    Nothing -> putStrLn ("ERROR: " <> BS.unpackChars targetInterface <> " not found") >> pure Nothing
    Just e -> do
      objID <- nextID counter
      Just <$> wlRegistryBind' e.name targetInterface e.version objID

eventLoop :: Socket -> IORef ObjectTracker -> IO ()
eventLoop wl_display refTracker = do
  putStrLn "Waiting for events..."
  msg <- receiveSocketData wl_display
  tracker <- readIORef refTracker
  unless (BS.null msg) $ do
    let events = runGet (parseEvents tracker) msg
    forM_ events $ \event -> do
      displayEvent event
      -- Handle events that need responses
      handleEventResponse wl_display event
  eventLoop wl_display refTracker

handleEventResponse :: Socket -> WaylandEvent -> IO ()
handleEventResponse wl_display (Event h e) =
  case cast e of
    Just (ping :: EventXdgWmBasePing) -> xdgWmBasePong wl_display h.objectID ping.serial
    Nothing -> return ()
handleEventResponse _ _ = return ()

data ObjectTracker = ObjectTracker
  { registryID :: Maybe LeWord32
  , wl_shmID :: Maybe LeWord32
  , xdg_wm_baseID :: Maybe LeWord32
  , wl_compositorID :: Maybe LeWord32
  , wl_surfaceID :: Maybe LeWord32
  , xdg_surfaceID :: Maybe LeWord32
  , xdg_toplevelID :: Maybe LeWord32
  }

main :: IO ()
main = do
  counter <- newIORef 2 -- Start from 2 as ID 1 is always wl_display
  wl_display <- wlDisplayConnect
  wl_registry <- wlDisplayGetRegistry wl_display =<< nextID counter
  trackerRef <- newIORef $ ObjectTracker (Just wl_registry) Nothing Nothing Nothing Nothing Nothing Nothing

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
  let bufferWidth :: Word32 = 117
  let bufferHeight :: Word32 = 150
  let stride :: Word32 = 117 * colorChannels
  let sharedPoolSize :: Word32 = bufferWidth * bufferHeight * colorChannels
  let poolName :: String = "saybar-shared-pool"

  void
    $ bracket
      (shmOpen poolName (ShmOpenFlags True True True True) 0600)
      (\fileDescriptor -> closeFd fileDescriptor >> shmUnlink poolName)
      (\fileDescriptor -> pure ())

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
  wl_shm <- bindToInterface globals "wl_shm" counter (wlRegistryBind' (\t objectID -> t{wl_shmID = objectID}))
  xdg_wm_base <- bindToInterface globals "xdg_wm_base" counter (wlRegistryBind' (\t objectID -> t{xdg_wm_baseID = objectID}))
  wl_compositor <- bindToInterface globals "wl_compositor" counter (wlRegistryBind' (\t objectID -> t{wl_compositorID = objectID}))

  wl_surface <- wlCompositorCreateSurface wl_display trackerRef (\t objectID -> t{wl_surfaceID = objectID}) =<< nextID counter
  xdg_surface <- xdgWmBaseCreateSurface wl_display trackerRef (\t objectID -> t{xdg_surfaceID = objectID}) =<< nextID counter
  xdg_toplevel <- xdgSurfaceGetTopLevel wl_display trackerRef (\t objectID -> t{xdg_toplevelID = objectID}) =<< nextID counter
  void $ wlSurfaceCommit wl_display trackerRef

  threadDelay maxBound
