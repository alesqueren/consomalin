{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan (crawl, makeTransaction, fetchSchedule, mymytest) where

import Protolude           hiding (Product)
import Conduit
import Control.Monad.Trans.Free.Church
import Data.Time

import Drive.Slot
import Drive.Product
import Drive.Attendance
import Drive.Transaction
import Drive.Crawl hiding (html)
import Drive.Crawl.Account
import Drive.Crawl.Auchan.Actions
import Drive.Crawl.Auchan.Product
import Drive.Crawl.Auchan.Page.Home as H
import Drive.Crawl.Auchan.Page.Login as LO
import Drive.Crawl.Auchan.Page.Basket as B
import Drive.Crawl.Auchan.Page.Landing as L
import Drive.Crawl.Auchan.Page.Category as C
import Drive.Crawl.Auchan.Page.Merchandising as M

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
    shopUrl <- L.doChooseDrive shopName
    homePage <- H.load shopUrl
    return $ map CategoryPage $ H.extractCategoryUrls homePage

crawlNode (CategoryPage url) =
  do
    $(logDebug) $ "[CategoryPage] " <> url
    products <- C.loadAndExtract url extractProducts
    return $ map AuchanDataPage products

crawlNode (AuchanDataPage sitePd) =
  do
    $(logDebug) $ "[AuchanDataPage] " <> show sitePd
    page <- M.load $ siteId sitePd
    let pd = makeProduct sitePd $ extractApiProduct page
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


mymytest :: IO ()
mymytest = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doTransaction2 acc)

doTransaction2 :: (MonadFree CrawlF cr) => Account -> ConduitM () Void cr ()
doTransaction2 acc = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ LO.login acc
  _ <- lift . fromF $ B.addToBasket ("800542", 1)
  return ()


makeTransaction :: Transaction -> IO ()
makeTransaction t = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doTransaction acc t)


fetchSchedule :: Attendance -> Day -> IO [Slot]
fetchSchedule attendance today = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doSchedule acc attendance today)
