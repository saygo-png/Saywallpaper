module Utils (padLen) where
import Data.Binary
import Relude
import Data.Bits

padLen :: Word32 -> Int64
padLen l = (.&.) (fromIntegral l + 3) (-4)
