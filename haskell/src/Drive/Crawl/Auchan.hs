{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan (crawl, doTransaction, doSchedule, mymytest) where

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
import Drive.Crawl.Auchan.Product
import Drive.Crawl.Auchan.Page.Home as H
import Drive.Crawl.Auchan.Page.Login as Lo
import Drive.Crawl.Auchan.Page.Basket as B
import Drive.Crawl.Auchan.Page.Landing as L
import Drive.Crawl.Auchan.Page.Schedule as S
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


-- TODO: rm
doTransaction2 :: (MonadFree CrawlF cr) => Account -> ConduitM () Void cr ()
doTransaction2 acc = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  _ <- lift . fromF $ B.addToBasket "666" 3
  _ <- lift . fromF $ B.getBasket
  return ()
-- TODO: rm
mymytest :: IO ()
mymytest = do
  acc <- makeAccount
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit (doTransaction2 acc)


doTransaction :: (MonadFree CrawlF cr) => Account -> Transaction -> ConduitM () Void cr ()
doTransaction acc t = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  _ <- mapM (\(p,q) -> lift $ fromF $ B.addToBasket p q) $ basket t
  _ <- lift . fromF $ B.load
  _ <- lift . fromF $ S.load
  _ <- lift . fromF $ selectSchedule $ slot t
  -- _ <- lift . fromF $ P.doValidatePayment
  return ()


doSchedule :: (MonadFree CrawlF cr) => Account -> Attendance -> Day -> ConduitM () Void cr [Slot]
doSchedule acc attendance today = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  -- a non empty basket is needed for getting schedule
  _ <- lift . fromF $ addToBasket "141418" 1
  _ <- lift . fromF $ B.load
  _ <- lift . fromF $ S.load
  slotInfo <- lift . fromF $ getSchedule
  return $ map (makeSlot attendance today) slotInfo 
