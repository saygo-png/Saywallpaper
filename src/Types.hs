module Types (Wayland, WaylandEnv (..), ObjectTracker (..), WaylandEvent (..), WaylandEventType (..)) where

import Data.Binary
import Headers
import Network.Socket
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)

type Wayland = ReaderT WaylandEnv IO

data WaylandEnv = WaylandEnv
  { tracker :: IORef ObjectTracker
  , socket :: Socket
  , counter :: IORef Word32
  , registryID :: Word32
  , wl_shmID :: Word32
  , wl_compositorID :: Word32
  , zwlr_layer_shell_v1ID :: Word32
  , parseEvents :: ObjectTracker -> Get [WaylandEvent]
  }

data ObjectTracker = ObjectTracker
  { wl_surfaceID :: Maybe Word32
  , wl_shm_poolID :: Maybe Word32
  , wl_buffer_A :: Maybe Word32
  , zwlr_layer_surface_v1ID :: Maybe Word32
  , zwlr_layer_surface_v1Serial :: TMVar Word32
  }

data WaylandEvent where
  Event :: (Binary a, WaylandEventType a, Typeable a) => Header -> a -> WaylandEvent
  EvEmpty :: (WaylandEventType a, Typeable a) => Header -> a -> WaylandEvent
  EvUnknown :: Header -> WaylandEvent

-- Type class for events that can describe themselves
class WaylandEventType a where
  formatEvent :: Word32 -> a -> String
