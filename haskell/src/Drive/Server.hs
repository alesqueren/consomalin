module Drive.Server (startSrv) where

import Prelude (read)
import Protolude hiding (get)

import Web.Scotty
import Network.Wai.Handler.Warp

import GHC.Exts 
import Control.Arrow
import Data.Aeson hiding (json)

import Drive.Product
import qualified Data.Text as T


startSrv :: Port -> IO()
startSrv port = do
  putStrLn $ "Listening on port " ++ show port
  scotty port $ do
    get "/search" $ do
      search <- param "s"
      pds <- liftIO $ searchProducts search
      json $ Object $ fromList $ map ((psId &&& toJSON) . summarize) pds
    get "/details" $ do
      pids <- param "pids" :: ActionM Text
      pds <- liftIO $ findProducts $ read $ T.unpack pids
      json $ Object $ fromList $ map ((psId &&& toJSON) . summarize) pds
