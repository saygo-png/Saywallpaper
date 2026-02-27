module Utils (padLen, putWlString, getWlString, nextID, nextID', mkMessage, wlDisplayID, waylandNull, headerSize, receiveSocketData, strReq) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy
import Network.Socket
import Network.Socket.ByteString.Lazy (recv)
import Relude hiding (ByteString, get, isPrefixOf, length, put, replicate)
import System.Console.ANSI
import Text.Printf
import Types

headerSize :: Int64
headerSize = 8 -- The header size is always 8 in Wayland

waylandNull :: Word32
waylandNull = 0 -- Nulls are just 0 in Wayland

wlDisplayID :: Word32
wlDisplayID = 1 -- wlDisplay always has ID 1 in Wayland

padLen :: Word32 -> Int64
padLen l = (.&.) (fromIntegral l + 3) (-4)

getColorize :: (IsString s, Semigroup s) => IO (ColorIntensity -> Color -> s -> s)
getColorize = do
  ansiSupport <- hNowSupportsANSI stdout
  pure
    $ if ansiSupport
      then \ci c t -> fromString (setSGRCode [SetColor Foreground ci c]) <> t <> fromString (setSGRCode [Reset])
      else const $ const id

strReq :: (String, Word32, String) -> String -> IO ()
strReq (object, objectID, method) text = do
  colorize <- getColorize
  putStrLn . colorize Vivid Magenta $ printf ("-> %s@%i.%s: " <> text) object objectID method

mkMessage :: Word32 -> Word16 -> ByteString -> ByteString
mkMessage objectID opCode messageBody =
  runPut $ do
    putWord32le objectID
    putWord16le opCode
    putWord16le $ fromIntegral (headerSize + length messageBody)
    putLazyByteString messageBody

receiveSocketData :: Socket -> IO ByteString
receiveSocketData sock = do
  liftIO $ recv sock 4096

nextID' :: IORef Word32 -> IO Word32
nextID' counter = do
  current <- readIORef counter
  modifyIORef counter (+ 1)
  return current

nextID :: IORef Word32 -> Wayland Word32
nextID = liftIO . nextID'

putWlString :: ByteString -> Put
putWlString bs = do
  let str = bs <> "\0"
  putWord32le (fromIntegral $ length str)
  putLazyByteString str
  let paddingBytes = padLen (fromIntegral $ length str) - length str
  replicateM_ (fromIntegral paddingBytes) (putWord8 0)

getWlString :: Get ByteString
getWlString = do
  len <- getWord32le
  str <- getLazyByteString (fromIntegral len)
  skip $ fromIntegral (padLen len - fromIntegral len)
  return str
