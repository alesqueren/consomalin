module Main where

import           Protolude
import           Drive.Rabbitmq

main :: IO ()
main = do
  listen TransactionResource
  forever $ threadDelay 10000000
