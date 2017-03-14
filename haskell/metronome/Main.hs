{-# LANGUAGE DeriveGeneric #-}
module Main where

import Protolude hiding (get, exp)

import Data.Aeson hiding (json)
import Web.Scotty
import Network.Wai.Handler.Warp

import Data.Attoparsec.Text
import Data.Time

import Drive.Utils
import Drive.Crawl.Auchan
import Drive.Crawl.Account
import Drive.Crawl.Auchan.Schedule


data Response = Response 
  { slots :: [Slot]
  , expiration :: UTCTime
  }
  deriving (Show, Generic)
instance ToJSON Response

main :: IO ()
main = do
  port <- fromEnvOr "SERVER_PORT" decimal 80 :: IO Int
  startSrv port

slotController :: ActionM ()
slotController = do
  now <- liftIO getCurrentTime

  si <- liftIO makeSchedule
  let s = map (makeSlot $ utctDay now) si

  let exp = addUTCTime (60*5) now

  json $ Response s exp

startSrv :: Port -> IO()
startSrv port = do
  putStrLn $ "Listening on port " ++ show port
  scotty port $ 
    get "/" slotController
