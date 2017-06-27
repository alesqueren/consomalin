{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Protolude hiding (get)
import           Web.Scotty
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types.Status
import           Data.Attoparsec.Text hiding (Done)
import           Data.Aeson hiding (json)
import qualified System.IO as SIO

import           Utils.Env
import           Drive.Basket
import           Drive.Crawl.Auchan
import           Drive.Bs.Rabbitmq


data BasketResponse = BasketResponse
  { message :: Text
  , basket :: MBasket
  }
  deriving (Show, Generic)
instance ToJSON BasketResponse

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  listen RegistrationResource processRegistrationMessage

  port <- fromEnvOr "SERVER_PORT" decimal 80
  startSrv port


processRegistrationMessage :: RegistrationMessage -> IO ()
processRegistrationMessage (RegistrationMessage uid) = do
  _ <- doRegisterIfNeeded uid
  return ()


startSrv :: Port -> IO()
startSrv port = do
  putStrLn $ "Listening on port " ++ show port
  scotty port $ do
    post "/user/:uid/prepareOrder" prepareController
    post "/user/:uid/order" orderController

prepareController :: ActionM ()
prepareController = do
  uid <- param "uid" :: ActionM Text
  b <- body

  case eitherDecode b of
    Left e -> do
      status status400
      text "Bad Request"
    Right t -> do
      mDiff <- liftIO $ prepareOrder uid t
      case mDiff of
        Nothing -> text "OK"
        Just diff -> do
          status status409
          json $ toJSON $ BasketResponse "basket errors" diff

orderController :: ActionM ()
orderController = do
  uid <- param "uid" :: ActionM Text
  b <- body

  case eitherDecode b of
    Left e -> do
      status status400
      text "Bad Request"
    Right t -> do
      mDiff <- liftIO $ order uid t
      -- putText (show t)
      -- putText (show mDiff)
      case mDiff of
        Nothing -> text "OK"
        Just diff -> do
          status status409
          json $ toJSON $ BasketResponse "basket errors" diff
