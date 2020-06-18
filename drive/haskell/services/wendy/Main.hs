{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Protolude hiding (get)
import           GHC.Exts
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


newtype OkResponse = OkResponse
  { transactionId :: Text
  }
  deriving (Show, Generic)
instance ToJSON OkResponse

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
    delete "/user/:uid/transaction/:tid" cancellationController

prepareController :: ActionM ()
prepareController = do
  uid <- param "uid" :: ActionM Text
  b <- body

  case eitherDecode b of
    Left _ -> do
      status status400
      text "Bad Request"
    Right t -> do
      mDiff <- liftIO $ prepareOrder uid t
      case mDiff of
        Nothing -> do
          codeAvailable <- liftIO $ checkPromoCode uid t
          if codeAvailable 
             then text "{\"welcomeCode\": true}"
             else text "{\"welcomeCode\": false}"
        Just diff -> do
          status status409
          json $ toJSON $ BasketResponse "basket errors" diff

orderController :: ActionM ()
orderController = do
  uid <- param "uid" :: ActionM Text
  b <- body

  case eitherDecode b of
    Left _ -> do
      status status400
      text "Bad Request"
    Right t -> do
      mDiff <- liftIO $ checkBasket uid t
      case mDiff of
        Nothing -> do
          tid <- liftIO $ order uid t
          json $ Object $ fromList [ ("basket" :: Text, toJSON $ Drive.Basket.basket t)
                                   , ("transactionId" :: Text, toJSON tid)
                                   ]
        Just diff -> do
          status status409
          json $ Object $ fromList [ ("message" :: Text, "basket errors")
                                   , ("basket" :: Text, toJSON diff)
                                   ]

cancellationController :: ActionM ()
cancellationController = do
  uid <- param "uid" :: ActionM Text
  tid <- param "tid" :: ActionM Text
  _ <- liftIO $ cancelTr uid tid
  text "OK"
