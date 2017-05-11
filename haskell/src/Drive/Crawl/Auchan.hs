{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan (crawl, makeTransaction, fetchSchedule) where

import           Protolude           hiding (Product)

import           Drive.Product
import           Drive.Crawl hiding (html)
import           Drive.Crawl.Auchan.ShopChoice
import           Drive.Crawl.Auchan.Home
import           Drive.Crawl.Auchan.Category
import           Drive.Crawl.Auchan.Product
import           Drive.Crawl.Auchan.Merchandising
import           Drive.Crawl.Auchan.Schedule

import           Conduit hiding (connect)
import           Control.Monad.Trans.Free.Church
import           Drive.Transaction

import           Drive.Crawl.Auchan.Actions
import           Drive.Crawl.Account

data Node = ShopChoicePage
            | HomePage TextURI
            | CategoryPage TextURI
            | AuchanDataPage SiteProduct
            | ProductPage Product
            deriving (Show, Typeable)

crawlNode :: Node -> Crawl [Node]
crawlNode ShopChoicePage =
  do
    $(logDebug) "[ShopChoicePage]"
    return [HomePage "Toulouse-954"]

crawlNode (HomePage shopName) =
  do
    $(logDebug) $ "[HomePage] " <> shopName
    shopUrl <- chooseDrive shopName
    catUrls <- fetchCategoryUrls shopUrl
    return $ map CategoryPage catUrls

crawlNode (CategoryPage url) =
  do
    $(logDebug) $ "[CategoryPage] " <> url
    pds <- fetchAuchanData url
    return $ map AuchanDataPage pds

crawlNode (AuchanDataPage sitePd) =
  do
    $(logDebug) $ "[AuchanDataPage] " <> show sitePd
    apiPd <- fetchProductAuchanData $ siteId sitePd
    let pd = makeProduct sitePd apiPd
    return [ProductPage pd]

crawlNode _ = return []

crawlTree :: (MonadFree CrawlF cr) => [Node] -> ConduitM () Product cr ()
crawlTree [] = return ()
crawlTree (x:xs) =
  case x of
    (ProductPage pd) -> do
      yield pd
      crawlTree xs
    _ -> do
      new <- lift . fromF $ crawlNode x
      crawlTree $ new ++ xs

crawl :: (MonadFree CrawlF cr) => ConduitM () Product cr ()
crawl = crawlTree [ShopChoicePage]

makeTransaction :: Transaction -> IO ()
makeTransaction t = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doTransaction acc t)
  return ()

fetchSchedule :: IO [SlotInfo]
fetchSchedule = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doSchedule acc)
