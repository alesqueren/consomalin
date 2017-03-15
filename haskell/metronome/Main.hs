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
import Drive.Crawl.Auchan.Schedule
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

  att <- liftIO $ findAttendance "balma"

  si <- liftIO makeSchedule
  let s = map (makeSlot att $ utctDay now) si

  if null s
    then raise "no slot found"
    else do
      let exp = addUTCTime (60*5) now
      json $ Response s exp
