{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan (auchanCrawl) where

import           Protolude           hiding (Product, inits)
import qualified Data.Text           as T
import           Data.List           (nub)

import           Drive.Product
import           Drive.Crawl hiding (html)
import           Drive.Crawl.Auchan.ShopChoice
import           Drive.Crawl.Auchan.Home
import           Drive.Crawl.Auchan.Category
import           Drive.Crawl.Auchan.Merchandising


data CrawlElement = ShopChoicePage
                  | HomePage TextURI
                  | CategoryPage TextURI
                  | AuchanDataPage AuchanData
                  | ProductPage Product
                  deriving (Show, Typeable)

crawl :: CrawlElement -> Crawl [CrawlElement]
crawl ShopChoicePage =
  do
    $(logDebug) "[ShopChoicePage]"
    return [HomePage "Toulouse-954"]

crawl (HomePage shopName) =
  do
    $(logDebug) $ T.append "[HomePage] " shopName
    shopUrl <- chooseDrive shopName
    catUrls <- fetchCategoryUrls shopUrl
    return $ map CategoryPage catUrls

crawl (CategoryPage url) =
  do
    $(logDebug) $ T.append "[CategoryPage] " url
    pds <- fetchAuchanData url
    return $ map AuchanDataPage pds

crawl (AuchanDataPage hot) =
  do
    $(logDebug) $ T.append "[AuchanDataPage] " $ show hot
    pd <- fetchProductAuchanData hot
    return [ProductPage pd]

crawl _ = return []

crawlList :: [CrawlElement] -> Crawl [Product]
crawlList [] = return []
crawlList (x:xs) =
  case x of
    (ProductPage pd) -> do
      pds <- crawlList xs
      return $ pd:pds
    _ -> do
      new <- crawl x
      crawlList $ new ++ xs

auchanCrawl :: Crawl [Product]
auchanCrawl = 
  do
    pds <- crawlList [ShopChoicePage]
    return $ nub pds
