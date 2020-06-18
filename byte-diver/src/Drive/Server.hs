module Drive.Server (startSrv) where

import Protolude hiding (get)

import Web.Scotty
import Network.Wai.Handler.Warp

import GHC.Exts 
import Control.Arrow
import Data.Aeson hiding (json)

import Drive.Product


startSrv :: Port -> IO()
startSrv port = do
  putStrLn $ "Listening on port " ++ show port
  scotty port $ do
    get "/search" $ do
      pids <- param "s"
      pds <- liftIO $ searchProducts pids
      json $ Object $ fromList $ map ((psId &&& toJSON) . summarize) pds
    get "/details" $ do
      search <- param "pids"
      pds <- liftIO $ findProducts search
      json $ Object $ fromList $ map ((psId &&& toJSON) . summarize) pds
