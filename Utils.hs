module Utils (swizzleRGBAtoBGRA, padLen, putWlString, getWlString) where

import Codec.Picture (Image (imageData), PixelRGBA8 (..))
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy qualified as BS
import Data.Vector.Storable qualified as VS
import Relude

padLen :: Word32 -> Int64
padLen l = (.&.) (fromIntegral l + 3) (-4)

swizzleRGBAtoBGRA :: Image PixelRGBA8 -> BS.ByteString
swizzleRGBAtoBGRA image =
  BS.pack . go . VS.toList $ imageData image
  where
    go [] = []
    go (r : g : b : a : rest) =
      let premul c = fromIntegral (fromIntegral c * fromIntegral a `div` 255 :: Word16)
       in premul b : premul g : premul r : a : go rest
    go _ = []

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
