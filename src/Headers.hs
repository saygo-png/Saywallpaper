module Headers (Header (..)) where

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
  toString (Header objectID opCode size) =
    printf "-- wl_header: objectID=%i opCode=%i size=%i" objectID opCode size
