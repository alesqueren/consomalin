module Main where

import           Protolude                 hiding (Product, put, get, modify)
import qualified System.IO as SIO
import           Data.Conduit.List         (chunksOf)
import qualified Data.Set                  as Set
import           Conduit
import           Control.Monad.Trans.State

import           Drive.Crawl
import           Drive.Crawl.Auchan
import           Drive.Product


productsInsert :: (MonadIO m) => ConduitM [Product] Void m ()
productsInsert = evalStateLC Set.empty (awaitForever ins) where
  ins pds = do
    seenIds <- lift get
    let dedup = filter (not . wasSeen seenIds) pds
    lift $ forM_ dedup (modify . Set.insert . pid)
    liftIO $ mongoInsert dedup
      where
        wasSeen s p = Set.member (pid p) s

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  runConduitCrawl $
    crawl
    .| chunksOf 50
    .| productsInsert
  -- man <- newManager tlsManagerSettings
  -- runNetCrawl man $ runConduit $
  --   crawl
  --   .| chunksOf 50
  --   .| productsInsert
