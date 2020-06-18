{-# LANGUAGE ScopedTypeVariables #-}

module Drive.Crawl.Auchan (auchanCrawl) where

import           Protolude           hiding (Product, inits)
import qualified Data.Text           as T
import qualified Data.List           as L

import           Drive.Product
import           Drive.Crawl hiding (html)
import           Drive.Crawl.Auchan.ShopChoice
import           Drive.Crawl.Auchan.Home
import           Drive.Crawl.Auchan.Category
import           Drive.Crawl.Auchan.Merchandising


class Crawlable a where
  crawl :: (Crawl cr) => a -> cr [a]

data CrawlElement = ShopChoicePage
                  | HomePage TextURI
                  | CategoryPage TextURI
                  | AuchanDataPage AuchanData
                  | ProductPage Product
                  deriving (Show, Typeable)

instance Crawlable CrawlElement where
  crawl ShopChoicePage = 
    do 
      putText "[ShopChoicePage]"
      return [HomePage "Toulouse-954"]

  crawl (HomePage shopName) = 
    do
      putText $ T.append "[HomePage] " shopName
      shopUrl <- chooseDrive shopName
      catUrls <- fetchCategoryUrls shopUrl
      return $ map CategoryPage catUrls

  crawl (CategoryPage url) = 
    do
      putText $ T.append "[CategoryPage] " url
      pds <- fetchAuchanData url
      return $ map AuchanDataPage pds

  crawl (AuchanDataPage hot) = 
    do
      putText $ T.append "[AuchanDataPage] " $ show hot
      pd <- fetchProductAuchanData hot
      return [ProductPage pd]

  crawl _ = return []

crawlList :: (Crawl cr)  => [CrawlElement] -> cr [Product]
crawlList [] = return []
crawlList (x:xs) = 
  case x of
    (ProductPage pd) -> do
      pds <- crawlList xs
      return $ pd:pds
    _ -> do
      new <- crawl x
      crawlList $ new ++ xs

auchanCrawl :: (Crawl cr)  => cr [Product]
auchanCrawl = 
  do 
    pds <- crawlList [ShopChoicePage]
    return $ L.nub pds
