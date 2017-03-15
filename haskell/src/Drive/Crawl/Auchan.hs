{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan (auchanCrawl, makeTransaction, makeSchedule) where

import           Protolude           hiding (Product)

import           Drive.Product
import           Drive.Crawl hiding (html)
import           Drive.Crawl.Auchan.ShopChoice
import           Drive.Crawl.Auchan.Home
import           Drive.Crawl.Auchan.Category
import           Drive.Crawl.Auchan.Merchandising
import           Drive.Crawl.Auchan.Schedule

import           Conduit hiding (connect)
import           Control.Monad.Trans.Free.Church
import           Drive.Transaction

import           Drive.Crawl.Auchan.Actions
import           Drive.Crawl.Account

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
    $(logDebug) $ "[HomePage] " <> shopName
    shopUrl <- chooseDrive shopName
    catUrls <- fetchCategoryUrls shopUrl
    return $ map CategoryPage catUrls

crawl (CategoryPage url) =
  do
    $(logDebug) $ "[CategoryPage] " <> url
    pds <- fetchAuchanData url
    return $ map AuchanDataPage pds

crawl (AuchanDataPage hot) =
  do
    $(logDebug) $ "[AuchanDataPage] " <> show hot
    pd <- fetchProductAuchanData hot
    return [ProductPage pd]

crawl _ = return []

crawlC :: (MonadFree CrawlF cr) => [CrawlElement] -> ConduitM () Product cr ()
crawlC [] = return ()
crawlC (x:xs) =
  case x of
    (ProductPage pd) -> do
      yield pd
      crawlC xs
    _ -> do
      new <- lift . fromF $ crawl x
      crawlC $ new ++ xs

auchanCrawl :: (MonadFree CrawlF cr) => ConduitM () Product cr ()
auchanCrawl = crawlC [ShopChoicePage]

makeTransaction :: Transaction -> IO ()
makeTransaction t = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doTransaction acc t)
  return ()

makeSchedule :: IO [SlotInfo]
makeSchedule = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doSchedule acc)
