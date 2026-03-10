module Config (colorChannels, bufferWidth, bufferHeight, poolName, colorFormat) where

import Relude
import Saywayland

bufferWidth :: WlInt
bufferWidth = 1920

bufferHeight :: WlInt
bufferHeight = 1080

poolName :: String
poolName = "saywallpaper-shared-pool"

colorFormat :: WlColorFormat
colorFormat = Argb8888

colorChannels :: WlInt
colorChannels = 4
