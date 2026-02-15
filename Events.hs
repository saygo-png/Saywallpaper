module Events (module Events) where

import Data.Binary
import Data.Binary qualified as Bin
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy hiding (map)
import Data.ByteString.Lazy.Internal
import Headers
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)
import Text.Printf
import Utils

-- import Text.Printf

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

-- wl_shm format event
newtype EventShmFormat = EventShmFormat {format :: Word32}

instance Binary EventShmFormat where
  get = EventShmFormat <$> getWord32le
  put (EventShmFormat fmt) = putWord32le fmt

-- wl_display error event
data EventDisplayError = EventDisplayError
  { errorObjectId :: Word32
  , errorCode :: Word32
  , errorMessage :: ByteString
  }

instance Binary EventDisplayError where
  get = do
    objId <- getWord32le
    code <- getWord32le
    msgLen <- getWord32le
    msg <- getLazyByteString $ padLen msgLen
    return $ EventDisplayError objId code msg
  put (EventDisplayError objId code msg) = do
    putWord32le objId
    putWord32le code
    let msgLen = fromIntegral $ length msg
    putWord32le msgLen
    putLazyByteString msg

-- wl_display delete_id event
newtype EventDisplayDeleteId = EventDisplayDeleteId {deletedId :: Word32}

instance Binary EventDisplayDeleteId where
  get = EventDisplayDeleteId <$> getWord32le
  put (EventDisplayDeleteId delId) = putWord32le delId

-- xdg_wm_base ping event
newtype EventXdgWmBasePing = EventXdgWmBasePing {serial :: Word32}

instance Binary EventXdgWmBasePing where
  get = EventXdgWmBasePing <$> getWord32le
  put (EventXdgWmBasePing s) = putWord32le s

-- Generic event that just stores raw bytes
newtype EventRaw = EventRaw {rawBytes :: ByteString}

data WaylandEvent
  = EvGlobal Header EventGlobal
  | EvShmFormat Header EventShmFormat
  | EvDisplayError Header EventDisplayError
  | EvDisplayDeleteId Header EventDisplayDeleteId
  | EvXdgWmBasePing Header EventXdgWmBasePing
  | EvUnknown Header

-- Format names for wl_shm
formatName :: Word32 -> String
formatName 0 = "ARGB8888"
formatName 1 = "XRGB8888"
formatName n = "format_" <> show n

-- Display event
displayEvent :: WaylandEvent -> IO ()
displayEvent (EvGlobal h e) =
  putStrLn
    $ printf
      "<- wl_registry@%i.global: name=%i interface=\"%s\" version=%i"
      h.objectID
      e.name
      (unpackChars e.interface)
      e.version
displayEvent (EvShmFormat h e) =
  putStrLn
    $ printf
      "<- wl_shm@%i.format: format=%s (%i)"
      h.objectID
      (formatName e.format)
      e.format
displayEvent (EvDisplayError h e) =
  putStrLn
    $ printf
      "<- wl_display@%i.error: object_id=%i code=%i message=\"%s\""
      h.objectID
      e.errorObjectId
      e.errorCode
      (unpackChars e.errorMessage)
displayEvent (EvDisplayDeleteId h e) =
  putStrLn
    $ printf
      "<- wl_display@%i.delete_id: id=%i"
      h.objectID
      e.deletedId
displayEvent (EvXdgWmBasePing h e) =
  putStrLn
    $ printf
      "<- xdg_wm_base@%i.ping: serial=%i"
      h.objectID
      e.serial
displayEvent (EvUnknown h) =
  putStrLn
    $ printf
      "<- unknown event: objectID=%i opCode=%i size=%i"
      h.objectID
      h.opCode
      h.size
