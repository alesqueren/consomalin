module Main where

import           Protolude
import           Drive.Rabbitmq
import           Drive.User
import           Drive.Crawl.Auchan

main :: IO ()
main = do
  listen TransactionResource processTransactionMessage
  forever $ threadDelay 100000000

processTransactionMessage :: TransactionMessage -> IO ()
processTransactionMessage tm = do
  mt <- findTransaction $ user tm
  case mt of
    Just t -> makeTransaction t
    Nothing -> putStrLn ("Error: no transaction found" :: Text)
