{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Events (module Events) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy hiding (map)
import Data.ByteString.Lazy.Internal
import GHC.Generics
import Headers
import Relude hiding (ByteString, Word32, get, isPrefixOf, put, replicate)
import Text.Printf
import Types
import Utils

-- Generic little-endian Binary deriving
type GBinaryLE :: forall {k}. (k -> Type) -> Constraint
class GBinaryLE f where
  ggetLE :: Get (f a)
  gputLE :: f a -> Put

instance GBinaryLE U1 where
  ggetLE = return U1
  gputLE U1 = return ()

instance (GBinaryLE a, GBinaryLE b) => GBinaryLE (a :*: b) where
  ggetLE = (:*:) <$> ggetLE <*> ggetLE
  gputLE (a :*: b) = gputLE a *> gputLE b

instance (GBinaryLE a) => GBinaryLE (M1 i c a) where
  ggetLE = M1 <$> ggetLE
  gputLE (M1 x) = gputLE x

-- Instances for primitive types
instance GBinaryLE (K1 i Word32) where
  ggetLE = K1 <$> getWord32le
  gputLE (K1 x) = putWord32le x

instance GBinaryLE (K1 i Int32) where
  ggetLE = K1 <$> getInt32le
  gputLE (K1 x) = putInt32le x

instance GBinaryLE (K1 i ByteString) where
  ggetLE = K1 <$> getWlString
  gputLE (K1 x) = putWlString x

-- Wayland array type - typed array with length-prefixed wire format
type role WlArray representational

newtype WlArray a = WlArray [a]
  deriving stock (Show)

instance GBinaryLE (K1 i (WlArray Word32)) where
  ggetLE = do
    len <- getWord32le
    bytes <- getLazyByteString $ padLen len
    let elems = runGet (replicateM (fromIntegral len `div` 4) getWord32le) bytes
    return $ K1 (WlArray elems)
  gputLE (K1 (WlArray xs)) = do
    let len = fromIntegral (Relude.length xs * 4) :: Word32
    putWord32le len
    mapM_ putWord32le xs

-- Newtype wrapper for deriving via
type role LittleEndian representational

newtype LittleEndian a = LittleEndian a

instance (Generic a, GBinaryLE (Rep a)) => Binary (LittleEndian a) where
  get = LittleEndian . to <$> ggetLE
  put (LittleEndian x) = gputLE (from x)

data EventGlobal = EventGlobal
  { name :: Word32
  , interface :: ByteString
  , version :: Word32
  }
  deriving stock (Generic, Show)
  deriving (Binary) via (LittleEndian EventGlobal)

instance WaylandEventType EventGlobal where
  formatEvent objId e =
    printf
      "wl_registry@%i.global: name=%i interface=\"%s\" version=%i"
      objId
      e.name
      (unpackChars e.interface)
      e.version

newtype EventShmFormat = EventShmFormat {format :: Word32}
  deriving stock (Generic, Show)
  deriving (Binary) via (LittleEndian EventShmFormat)

instance WaylandEventType EventShmFormat where
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
  deriving (Binary) via (LittleEndian EventDisplayError)

instance WaylandEventType EventDisplayError where
  formatEvent objId e =
    printf
      "wl_display@%i.error: object_id=%i code=%i message=\"%s\""
      objId
      e.errorObjectId
      e.errorCode
      (unpackChars e.errorMessage)

newtype EventDisplayDeleteId = EventDisplayDeleteId {deletedId :: Word32}
  deriving stock (Generic, Show)
  deriving (Binary) via (LittleEndian EventDisplayDeleteId)

instance WaylandEventType EventDisplayDeleteId where
  formatEvent objId e =
    printf "wl_display@%i.delete_id: id=%i" objId e.deletedId

data EventBufferRelease = EventBufferRelease
  deriving stock (Generic, Show)

instance WaylandEventType EventBufferRelease where
  formatEvent objId _e =
    printf "wl_buffer@%i.release: buffer released" objId

data EventWlrLayerSurfaceConfigure = EventWlrLayerSurfaceConfigure
  { serial :: Word32
  , width :: Word32
  , height :: Word32
  }
  deriving stock (Generic, Show)
  deriving (Binary) via (LittleEndian EventWlrLayerSurfaceConfigure)

instance WaylandEventType EventWlrLayerSurfaceConfigure where
  formatEvent objId e =
    printf
      "zwlr_layer_surface_v1@%i.configure: serial=%i width=%i height=%i"
      objId
      e.serial
      e.width
      e.height

-- Single display function that works for all events
displayEvent :: WaylandEvent -> IO ()
displayEvent (Event h e) = putStrLn $ "<- " <> formatEvent h.objectID e
displayEvent (EvEmpty h e) = putStrLn $ "<- " <> formatEvent h.objectID e
displayEvent (EvUnknown h) = putStrLn $ printf "<- unknown event: objectID=%i opCode=%i size=%i" h.objectID h.opCode h.size

-- Format names for wl_shm
formatName :: Word32 -> String
formatName 0 = "ARGB8888"
formatName 1 = "XRGB8888"
formatName n = "format_" <> show n

-- Helper to create events
mkEvent :: (Binary a, WaylandEventType a, Typeable a) => Header -> a -> WaylandEvent
mkEvent = Event
