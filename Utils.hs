module Utils (padLen, putWlString, getWlString) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy qualified as BS
import Relude

padLen :: Word32 -> Int64
padLen l = (.&.) (fromIntegral l + 3) (-4)

putWlString :: BS.ByteString -> Put
putWlString bs = do
  let str = bs <> "\0" -- null terminator
  putWord32le (fromIntegral $ BS.length str)
  putLazyByteString str
  let paddingBytes = padLen (fromIntegral $ BS.length str) - fromIntegral (BS.length str)
  replicateM_ (fromIntegral paddingBytes) (putWord8 0)

getWlString :: Get BS.ByteString
getWlString = do
  len <- getWord32le
  str <- getLazyByteString (fromIntegral len)
  skip $ fromIntegral (padLen len - fromIntegral len)
  return str
