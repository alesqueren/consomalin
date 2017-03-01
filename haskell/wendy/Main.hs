module Main where

import           Protolude
import           Drive.User
import           Drive.Rabbitmq

main :: IO ()
main = do
  --t <- findTransaction "az@hotmail.fr"
  --putStrLn (show t :: Text)

  listen TransactionResource
