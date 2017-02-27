module Main where

import           Protolude
import           Drive.User

main :: IO ()
main = do
  t <- findTransaction "az@hotmail.fr"
  putStr (show t :: Text)
