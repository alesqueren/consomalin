module Main where

import           Protolude
import           Drive.Bs.Rabbitmq
import           Drive.Transaction
import           Drive.Crawl.Auchan

main :: IO ()
main = do
  listen TransactionResource processTransactionMessage
  forever $ threadDelay 100000000

processTransactionMessage :: TransactionMessage -> IO ()
processTransactionMessage (TransactionMessage uid tid) = do
  mt <- mongoFind uid tid
  case mt of
    Just t -> do
      makeTransaction t
      mongoSet uid tid Done
    Nothing -> putStrLn ("Error: no transaction found" :: Text)
