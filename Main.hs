module Main (main) where

import Control.Exception
import Data.Binary hiding (get, put)
import Data.Binary qualified as Bin
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy hiding (map)
import Data.ByteString.Lazy.Internal
import Data.Int
import Network.Socket
import Network.Socket.ByteString.Lazy
import Relude hiding (isPrefixOf, ByteString, get, put, replicate)
import System.Environment (getEnv)
import System.Posix.IO
import System.Posix.SharedMem
import Text.Printf

padLen :: Word32 -> Int64
padLen l = (.&.) (fromIntegral l + 3) (-4)

data Header = Header
  { objectID :: Word32
  , opCode :: Word16
  , size :: Word16
  }

-- The byteorder is based on the host system, so this doesn't adhere to the protocol
-- as little endian is used no matter what. The program will only work on x86/x64 systems.
newtype LeWord32 = LeWord32 Word32
  deriving newtype (Num, PrintfArg, Show, Eq, Ord)

instance Binary LeWord32 where
  put :: LeWord32 -> Put
  put = putWord32le . coerce
  get :: Get LeWord32
  get = LeWord32 <$> getWord32le

instance Binary Header where
  put :: Header -> Put
  put header = do
    putWord32le header.objectID
    putWord16le header.opCode
    putWord16le header.size
  get :: Get Header
  get = Header <$> getWord32le <*> getWord16le <*> getWord16le

instance ToString Header where
  toString :: Header -> String
  toString (Header oi op sz) =
    printf "-- wl_header: objectID=%i opCode=%i size=%i" oi op sz

data EventGlobal = EventGlobal
  { name :: Word32
  , interface :: ByteString
  , version :: Word32
  }

instance Binary EventGlobal where
  put :: EventGlobal -> Put
  put eventGlobal = do
    Bin.put eventGlobal.name
    Bin.put eventGlobal.interface
    Bin.put eventGlobal.version
  get :: Get EventGlobal
  get = do
    name <- getWord32le
    interface_len <- getWord32le
    interface <- getLazyByteString $ padLen interface_len
    let version = getWord32le
    EventGlobal name interface <$> version

newtype DebugEventGlobal = DebugEventGlobal (Header, EventGlobal)

instance ToString DebugEventGlobal where
  toString :: DebugEventGlobal -> String
  toString (DebugEventGlobal (h, e)) =
    printf "<- wl_registry@%i.global: name=%i interface=\"%s\" version=%i" h.objectID e.name (unpackChars e.interface) e.version

parseMessage :: (Binary a) => Get [(Header, a)]
parseMessage = do
  isEmpty >>= \case
    True -> return []
    False -> do
      message <- Bin.get
      messages <- parseMessage
      return (message : messages)

wlDisplayConnect :: IO Socket
wlDisplayConnect = do
  xdg_runtime_dir <- getEnv "XDG_RUNTIME_DIR"
  wayland_display <- getEnv "WAYLAND_DISPLAY"
  let path = xdg_runtime_dir <> "/" <> wayland_display
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  return sock

sendMessage :: Socket -> ByteString -> IO ()
sendMessage = sendAll

receiveSocketData :: Socket -> IO ByteString
receiveSocketData sock = recv sock 4096

wlDisplayGetRegistry :: Socket -> LeWord32 -> IO LeWord32
wlDisplayGetRegistry wl_display newObjectID = do
  let wl_header = encode (Header 1 1 12, newObjectID)
  sendMessage wl_display wl_header
  putStrLn "  --> wl_display@1.get_registry: wl_registry=2"
  return newObjectID

nextID :: IORef LeWord32 -> IO LeWord32
nextID counter = do
  current <- readIORef counter
  modifyIORef counter (+ 1)
  return current

wlRegistryBind :: Socket -> LeWord32 -> Word32 -> ByteString -> Word32 -> LeWord32 -> IO ()
wlRegistryBind sock registryID globalName interfaceName interfaceVersion newObjectID = do
  let interfaceStr = interfaceName <> "\0" -- Null-terminated string
  let interfaceLen = fromIntegral $ Data.ByteString.Lazy.length interfaceStr
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
  sendMessage sock message
  putStrLn
    $ printf
      " --> wl_registry@%i.bind: name=%i interface=\"%s\" version=%i id=%i"
      registryID
      globalName
      (unpackChars interfaceName)
      interfaceVersion
      newObjectID

findInterface :: [(Header, EventGlobal)] -> ByteString -> Maybe EventGlobal
findInterface messages targetInterface = 
  let target = targetInterface <> "\0"
  in Relude.find (\(_, e) -> target `isPrefixOf` e.interface) messages >>= Just . snd

main :: IO ()
main = do
  counter <- newIORef 2 -- Start from 2 as ID 1 is always wl_display
  wl_display <- wlDisplayConnect
  wl_registry <- wlDisplayGetRegistry wl_display =<< nextID counter
  let wlRegistryBind' = wlRegistryBind wl_display wl_registry

  socketData <- receiveSocketData wl_display
  let messages = runGet (parseMessage :: Get [(Header, EventGlobal)]) socketData
  mapM_ (putStrLn . toString . DebugEventGlobal) messages

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
  case findInterface messages "wl_shm" of
    Nothing -> putStrLn "ERROR: wl_shm not found"
    Just e -> do
      objID <- nextID counter
      wlRegistryBind' e.name "wl_shm" e.version objID

  case findInterface messages "xdg_wm_base" of
    Nothing -> putStrLn "ERROR: xdg_wm_base not found"
    Just e -> do
      objID <- nextID counter
      wlRegistryBind' e.name "xdg_wm_base" e.version objID

  case findInterface messages "wl_compositor" of
    Nothing -> putStrLn "ERROR: wl_compositor not found"
    Just e -> do
      objID <- nextID counter
      wlRegistryBind' e.name "wl_compositor" e.version objID

  close wl_display
