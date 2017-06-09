module Main where

import           Protolude hiding (get)
import           Prelude (read)
import qualified System.IO as SIO
import           GHC.Exts 
import           Web.Scotty
import           Network.Wai.Handler.Warp
import           Control.Arrow
import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Data.Aeson hiding (json)

import           Utils.Env
import           Drive.Product as P
import           Drive.ViewProduct as V


main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  port <- fromEnvOr "SERVER_PORT" decimal 80
  startSrv port

startSrv :: Port -> IO()
startSrv port = do
  putStrLn $ "Listening on port " ++ show port
  scotty port $ do
    get "/search" searchController
    get "/details" detailsController

searchController :: ActionM ()
searchController = do
  search <- param "s"
  pds <- liftIO $ P.mongoSearch search
  json $ Object $ fromList $ map ((V.id &&& toJSON) . P.summarize) pds

detailsController :: ActionM ()
detailsController = do
  pids <- param "pids" :: ActionM Text
  pds <- liftIO $ P.mongoFind $ read $ T.unpack pids
  json $ Object $ fromList $ map ((V.id &&& toJSON) . P.summarize) pds
