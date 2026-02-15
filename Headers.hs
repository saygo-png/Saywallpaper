module Headers (Header(..), LeWord32(..)) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)
import Text.Printf

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
