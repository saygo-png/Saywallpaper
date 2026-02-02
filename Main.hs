import Relude

data AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

main :: IO ()
main = putStrLn "Hello world!"
