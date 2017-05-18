{-# LANGUAGE DeriveGeneric #-}

module Main where

import Protolude hiding (get, exp)

import Data.Aeson hiding (json)
import Web.Scotty
import Network.Wai.Handler.Warp

import Data.Attoparsec.Text
import Data.Time

import Utils.Env
import Drive.Slot
import Drive.Crawl.Auchan
import Drive.Attendance

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

startSrv :: Port -> IO()
startSrv port = do
  putStrLn $ "Listening on port " ++ show port
  scotty port $ 
    get "/" slotController


slotController :: ActionM ()
slotController = do
  now <- liftIO getCurrentTime
  attendance <- liftIO $ mongoFind "balma"
  slots <- liftIO $ fetchSchedule attendance $ utctDay now
  if null slots
    then raise "no slot found"
    else do
      let exp = addUTCTime (60*5) now
      json $ Response slots exp
