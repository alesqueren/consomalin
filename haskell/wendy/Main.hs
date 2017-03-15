module Main where

import           Protolude
import           Drive.Rabbitmq
import           Drive.Transaction
import           Drive.Crawl.Auchan

main :: IO ()
main = do
  listen TransactionResource processTransactionMessage
  forever $ threadDelay 100000000

processTransactionMessage :: TransactionMessage -> IO ()
processTransactionMessage (TransactionMessage uid tid) = do
  mt <- findTransaction uid tid
  case mt of
    Just t -> do
      makeTransaction t
      changeStatus uid tid Done
    Nothing -> putStrLn ("Error: no transaction found" :: Text)
