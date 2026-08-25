module Config (colorChannels, bufferWidth, bufferHeight, poolName, colorFormat) where

import Relude
import Saywayland

bufferWidth :: Int
bufferWidth = 1920

bufferHeight :: Int
bufferHeight = 1080

poolName :: String
poolName = "saywallpaper-shared-pool"

colorFormat :: Enum_wl_shm_format
colorFormat = Enum_wl_shm_format_argb8888

colorChannels :: Int
colorChannels = 4
