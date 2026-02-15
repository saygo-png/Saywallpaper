module Main (main) where

import Control.Exception
import Data.Binary hiding (get, put)
import Data.Binary qualified as Bin
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Internal qualified as BS
import Data.Int
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
  let bodySize = fromIntegral header.size - 8
  case (header.objectID, header.opCode) of
    -- wl_display events (object 1)
    (1, 0) -> EvDisplayError header <$> Bin.get
    (1, 1) -> EvDisplayDeleteId header <$> Bin.get
    -- wl_registry events (object 2)
    (2, 0) -> EvGlobal header <$> Bin.get
    (2, 1) -> do
      -- registry.global_remove event
      skip bodySize
      return $ EvUnknown header

    -- Check if this is wl_shm
    _
      | Just oid <- tracker.wlShmId
      , oid == coerce header.objectID && header.opCode == 0 ->
          EvShmFormat header <$> Bin.get
    -- Check if this is xdg_wm_base
    _
      | Just oid <- tracker.xdgWmBaseId
      , oid == coerce header.objectID && header.opCode == 0 ->
          EvXdgWmBasePing header <$> Bin.get
    -- Unknown event - skip body
    _ -> do
      skip bodySize
      return $ EvUnknown header

parseEvents :: ObjectTracker -> Get [WaylandEvent]
parseEvents tracker = do
  isEmpty >>= \case
    True -> return []
    False -> do
      event <- parseEvent tracker
      rest <- parseEvents tracker
      return (event : rest)

wlDisplayGetRegistry :: Socket -> LeWord32 -> IO LeWord32
wlDisplayGetRegistry wl_display newObjectID = do
  let wl_header = encode (Header 1 1 12, newObjectID)
  sendAll wl_display wl_header
  putStrLn "  --> wl_display@1.get_registry: wl_registry=2"
  return newObjectID

xdgWmBasePong :: Socket -> Word32 -> Word32 -> IO ()
xdgWmBasePong sock xdgWmBaseId serial = do
  let message = runPut $ do
        putWord32le xdgWmBaseId
        putWord16le 0 -- pong opcode
        putWord16le 12 -- message size (header + serial)
        putWord32le serial
  sendAll sock message
  putStrLn $ printf "-> xdg_wm_base@%i.pong: serial=%i" xdgWmBaseId serial

nextID :: IORef LeWord32 -> IO LeWord32
nextID counter = do
  current <- readIORef counter
  modifyIORef counter (+ 1)
  return current

wlRegistryBind :: Socket -> LeWord32 -> Word32 -> ByteString -> Word32 -> LeWord32 -> IO (LeWord32)
wlRegistryBind sock registryID globalName interfaceName interfaceVersion newObjectID = do
  let interfaceStr = interfaceName <> "\0" -- Null-terminated string
  let interfaceLen = fromIntegral $ length interfaceStr
  let paddedLen = padLen interfaceLen
  let paddingBytes = paddedLen - fromIntegral interfaceLen

  let messageSize = header + name + string_len + paddedLen + version + new_id
        where
          header :: Int64 = 8
          name :: Int64 = 4
          string_len :: Int64 = 4
          version :: Int64 = 4
          new_id :: Int64 = 4
  let message = runPut $ do
        -- Header
        putWord32le (coerce registryID)
        putWord16le 0 -- opcode (bind = 0)
        putWord16le (fromIntegral messageSize) -- message size
        -- bind parameters
        putWord32le globalName -- name
        putWord32le interfaceLen -- string length
        putLazyByteString interfaceStr -- interface string
        replicateM_ (fromIntegral paddingBytes) (putWord8 0) -- padding
        putWord32le interfaceVersion -- version
        putWord32le (coerce newObjectID) -- new_id
  sendAll sock message
  putStrLn
    $ printf
      " --> wl_registry@%i.bind: name=%i interface=\"%s\" version=%i id=%i"
      registryID
      globalName
      (BS.unpackChars interfaceName)
      interfaceVersion
      newObjectID
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

eventLoop :: Socket -> ObjectTracker -> IO ()
eventLoop wl_display tracker = do
  msg <- receiveSocketData wl_display
  unless (BS.null msg) $ do
    let events = runGet (parseEvents tracker) msg
    forM_ events $ \event -> do
      displayEvent event
      -- Handle events that need responses
      case event of
        EvXdgWmBasePing h e -> do
          xdgWmBasePong wl_display h.objectID e.serial
        _ -> return ()
    eventLoop wl_display tracker

data ObjectTracker = ObjectTracker
  { registryId :: Maybe LeWord32
  , wlShmId :: Maybe LeWord32
  , xdgWmBaseId :: Maybe LeWord32
  , wlCompositorId :: Maybe LeWord32
  }

main :: IO ()
main = do
  counter <- newIORef 2 -- Start from 2 as ID 1 is always wl_display
  wl_display <- wlDisplayConnect
  wl_registry <- wlDisplayGetRegistry wl_display =<< nextID counter

  socketData <- receiveSocketData wl_display
  let initialEvents = runGet (parseEvents $ ObjectTracker (Just wl_registry) Nothing Nothing Nothing) socketData
  mapM_ displayEvent initialEvents

  -- Extract globals from events
  let globals = [(h, e) | EvGlobal h e <- initialEvents]

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

  -- Bind to the required interfaces
  let wlRegistryBind' = wlRegistryBind wl_display wl_registry
  wl_shm <- bindToInterface globals "wl_shm" counter wlRegistryBind'
  xdg_wm_base <- bindToInterface globals "xdg_wm_base" counter wlRegistryBind'
  wl_compositor <- bindToInterface globals "wl_compositor" counter wlRegistryBind'

  -- Read responses from bind operations
  putStrLn "\n--- Reading bind responses ---"
  finally (eventLoop wl_display (ObjectTracker (Just wl_registry) Nothing Nothing Nothing)) (close wl_display)
