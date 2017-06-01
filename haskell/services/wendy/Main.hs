module Main where

import           Protolude hiding (get)
import           Web.Scotty
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types.Status
import           Data.Attoparsec.Text hiding (Done)
import           Data.Aeson hiding (json)

import           Utils.Env
import           Drive.Crawl.Auchan
import           Drive.Bs.Rabbitmq

main :: IO ()
main = do
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
  eBasket <- liftIO $ prepareOrder uid
  case eBasket of
    Left err -> do
      status status409 
      json err
    Right basket ->
      json $ toJSON basket

orderController :: ActionM ()
orderController = do
  uid <- param "uid" :: ActionM Text
  eBasket <- liftIO $ order uid
  case eBasket of
    Left err -> do
      status status409 
      json err
    Right basket ->
      json $ toJSON basket
