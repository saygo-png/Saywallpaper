{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Events (module Events) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy hiding (map)
import Data.ByteString.Lazy.Internal
import Headers
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)
import Text.Printf
import Utils

-- Helper functions for Wayland wire format
getWlString :: Get ByteString
getWlString = do
  len <- getWord32le
  getLazyByteString (padLen len)

putWlString :: ByteString -> Put
putWlString bs = do
  putWord32le (fromIntegral $ length bs)
  putLazyByteString bs

getWlArray :: Get ByteString
getWlArray = do
  len <- getWord32le
  getLazyByteString (padLen len)

putWlArray :: ByteString -> Put
putWlArray bs = do
  putWord32le (fromIntegral $ length bs)
  putLazyByteString bs

-- Type class for events that can describe themselves
class WaylandEventType a where
  eventName :: a -> String
  formatEvent :: Word32 -> a -> String -- objectID -> event -> formatted string

-- Simple wrapper for events with auto-derived instances
data EventGlobal = EventGlobal
  { name :: Word32
  , interface :: ByteString
  , version :: Word32
  }
  deriving stock (Generic, Show)

instance Binary EventGlobal where
  get = EventGlobal <$> getWord32le <*> getWlString <*> getWord32le
  put e = putWord32le e.name *> putWlString e.interface *> putWord32le e.version

instance WaylandEventType EventGlobal where
  eventName _ = "global"
  formatEvent objId e =
    printf
      "wl_registry@%i.global: name=%i interface=\"%s\" version=%i"
      objId
      e.name
      (unpackChars e.interface)
      e.version

newtype EventShmFormat = EventShmFormat {format :: Word32}
  deriving stock (Generic, Show)
  deriving newtype (Binary)

instance WaylandEventType EventShmFormat where
  eventName _ = "format"
  formatEvent objId e =
    printf
      "wl_shm@%i.format: format=%s (%i)"
      objId
      (formatName e.format)
      e.format

data EventDisplayError = EventDisplayError
  { errorObjectId :: Word32
  , errorCode :: Word32
  , errorMessage :: ByteString
  }
  deriving stock (Generic, Show)

instance Binary EventDisplayError where
  get = EventDisplayError <$> getWord32le <*> getWord32le <*> getWlString
  put e = putWord32le e.errorObjectId *> putWord32le e.errorCode *> putWlString e.errorMessage

instance WaylandEventType EventDisplayError where
  eventName _ = "error"
  formatEvent objId e =
    printf
      "wl_display@%i.error: object_id=%i code=%i message=\"%s\""
      objId
      e.errorObjectId
      e.errorCode
      (unpackChars e.errorMessage)

newtype EventDisplayDeleteId = EventDisplayDeleteId {deletedId :: Word32}
  deriving stock (Generic, Show)
  deriving newtype (Binary)

instance WaylandEventType EventDisplayDeleteId where
  eventName _ = "delete_id"
  formatEvent objId e =
    printf "wl_display@%i.delete_id: id=%i" objId e.deletedId

newtype EventXdgWmBasePing = EventXdgWmBasePing {serial :: Word32}
  deriving stock (Generic, Show)
  deriving newtype (Binary)

instance WaylandEventType EventXdgWmBasePing where
  eventName _ = "ping"
  formatEvent objId e =
    printf "xdg_wm_base@%i.ping: serial=%i" objId e.serial

newtype EventXdgSurfaceConfigure = EventXdgSurfaceConfigure {serial :: Word32}
  deriving stock (Generic, Show)
  deriving newtype (Binary)

instance WaylandEventType EventXdgSurfaceConfigure where
  eventName _ = "configure"
  formatEvent objId e =
    printf "xdg_surface@%i.configure: serial=%i" objId e.serial

data EventXdgToplevelConfigure = EventXdgToplevelConfigure
  { width :: Int32
  , height :: Int32
  , states :: ByteString
  }
  deriving stock (Generic, Show)

instance Binary EventXdgToplevelConfigure where
  get = EventXdgToplevelConfigure <$> getInt32le <*> getInt32le <*> getWlArray
  put e = putInt32le e.width *> putInt32le e.height *> putWlArray e.states

instance WaylandEventType EventXdgToplevelConfigure where
  eventName _ = "configure"
  formatEvent objId e =
    printf
      "xdg_toplevel@%i.configure: width=%i height=%i states=%i bytes"
      objId
      e.width
      e.height
      (length e.states)

data EventXdgToplevelClose = EventXdgToplevelClose
  deriving stock (Generic, Show)

instance Binary EventXdgToplevelClose

instance WaylandEventType EventXdgToplevelClose where
  eventName _ = "close"
  formatEvent objId _ = printf "xdg_toplevel@%i.close" objId

data EventXdgToplevelConfigureBounds = EventXdgToplevelConfigureBounds
  { boundsWidth :: Int32
  , boundsHeight :: Int32
  }
  deriving stock (Generic, Show)

instance Binary EventXdgToplevelConfigureBounds

instance WaylandEventType EventXdgToplevelConfigureBounds where
  eventName _ = "configure_bounds"
  formatEvent objId e =
    printf
      "xdg_toplevel@%i.configure_bounds: width=%i height=%i"
      objId
      e.boundsWidth
      e.boundsHeight

newtype EventXdgToplevelWmCapabilities = EventXdgToplevelWmCapabilities
  {capabilities :: ByteString}
  deriving stock (Generic, Show)

instance Binary EventXdgToplevelWmCapabilities where
  get = EventXdgToplevelWmCapabilities <$> getWlArray
  put e = putWlArray e.capabilities

instance WaylandEventType EventXdgToplevelWmCapabilities where
  eventName _ = "wm_capabilities"
  formatEvent objId e =
    printf
      "xdg_toplevel@%i.wm_capabilities: capabilities=%i bytes"
      objId
      (length e.capabilities)

-- GADT for type-safe event variants
data WaylandEvent where
  Event :: (Binary a, WaylandEventType a, Typeable a) => Header -> a -> WaylandEvent
  EvUnknown :: Header -> WaylandEvent

-- Single display function that works for all events
displayEvent :: WaylandEvent -> IO ()
displayEvent (Event h e) = putStrLn $ "<- " <> formatEvent h.objectID e
displayEvent (EvUnknown h) =
  putStrLn
    $ printf
      "<- unknown event: objectID=%i opCode=%i size=%i"
      h.objectID
      h.opCode
      h.size

-- Format names for wl_shm
formatName :: Word32 -> String
formatName 0 = "ARGB8888"
formatName 1 = "XRGB8888"
formatName n = "format_" <> show n

-- Helper to create events
mkEvent :: (Binary a, WaylandEventType a, Typeable a) => Header -> a -> WaylandEvent
mkEvent = Event
