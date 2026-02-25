module Config (colorChannels, bufferWidth, bufferHeight, poolName, colorFormat) where

import Relude

bufferWidth :: Word32
bufferWidth = 1920

bufferHeight :: Word32
bufferHeight = 25

poolName :: String
poolName = "saybar-shared-pool"

colorFormat :: Word32
colorFormat = 0 -- ARGB8888

colorChannels :: Word32
colorChannels = 4
