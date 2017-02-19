module Main where

import           Protolude hiding (Product)
import           Drive.Crawl
import           Drive.Crawl.Auchan
import           Drive.Product

main :: IO ()
main = do
  man <- newManager tlsManagerSettings
  cr <- try $ runNetCrawl man auchanCrawl
  case (cr :: Either SomeException [Product]) of
    Left e -> putText $ show e
    Right pds -> insertProducts pds
