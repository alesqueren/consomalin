module Main where

import           Protolude
import           Server
import           Drive.Utils
import           Data.Attoparsec.Text

main :: IO ()
main = do
  port <- fromEnvOr "SERVER_PORT" decimal 80 :: IO Int
  startSrv port
