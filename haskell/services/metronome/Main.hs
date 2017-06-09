{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Protolude hiding (get, exp)
import qualified System.IO as SIO
import           Data.Aeson hiding (json)
import           Web.Scotty
import           Network.Wai.Handler.Warp
import           Data.Attoparsec.Text
import           Data.Time

import           Utils.Env
import           Drive.Slot
import           Drive.Crawl.Auchan

data Response = Response
  { slots :: [Slot]
  , expiration :: UTCTime
  }
  deriving (Show, Generic)
instance ToJSON Response

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  port <- fromEnvOr "SERVER_PORT" decimal 80
  startSrv port

startSrv :: Port -> IO()
startSrv port = do
  putStrLn $ "Listening on port " ++ show port
  scotty port $ 
    get "/" slotController

slotController :: ActionM ()
slotController = do
  (slots, exp) <- liftIO fetchSchedule 
  if null slots
    then raise "no slot found"
    else
      json $ Response slots exp
