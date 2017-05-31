{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan (crawl, doRegisterIfNeeded, fetchSchedule, prepareOrder, order) where

import Protolude           hiding (Product)
import Conduit
import Control.Monad.Trans.Free.Church
import Data.Time

import Drive.Slot
import Drive.Product
import Drive.Attendance as Att
import Drive.ConsoBasket as CB
import Drive.DriveBasket as DB
import Drive.Crawl hiding (html)
import Drive.Crawl.Account as A
import Drive.Crawl.Auchan.Product
import Drive.Crawl.Auchan.Page.Home as H
import Drive.Crawl.Auchan.Page.Login as Lo
import Drive.Crawl.Auchan.Page.Basket as B
import Drive.Crawl.Auchan.Page.Landing as L
import Drive.Crawl.Auchan.Page.Register as R
import Drive.Crawl.Auchan.Page.Schedule as S
import Drive.Crawl.Auchan.Page.Category as C
import Drive.Crawl.Auchan.Page.Merchandising as M

data RegistrationException = RegistrationException
  deriving (Show, Typeable)
instance Exception RegistrationException

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
    catUrls <- H.extractCategoryUrls shopUrl
    return $ map CategoryPage catUrls

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


doRegistration :: (MonadFree CrawlF cr) => UserData -> ConduitM () Void cr ()
doRegistration u = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ R.register u
  return ()

doPrepareOrder :: (MonadFree CrawlF cr) => Account -> ConsoBasket -> ChoosenSlot -> ConduitM () Void cr DriveBasket
doPrepareOrder acc (ConsoBasket pds) (ChoosenSlot slotId) = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  _ <- mapM (lift . fromF . B.addToBasket) pds
  b <- lift . fromF $ B.getBasket
  _ <- lift . fromF $ S.load
  _ <- lift . fromF $ selectSchedule slotId
  return b

-- doTransaction :: (MonadFree CrawlF cr) => Account -> GT.Transaction -> ConduitM () Void cr ()
-- doTransaction acc t = do
--   _ <- lift . fromF $ doChooseDrive "Toulouse-954"
--   _ <- lift . fromF $ Lo.login acc
--   -- _ <- mapM (lift . fromF . B.addToBasket) $ GB.products $ GT.basket t
--   _ <- lift . fromF $ B.load
--   _ <- lift . fromF $ S.load
--   _ <- lift . fromF $ selectSchedule $ GT.slot t
--   -- _ <- lift . fromF $ P.doValidatePayment
--   return ()

doRegisterIfNeeded :: Text -> IO Account
doRegisterIfNeeded uid = do
  macc <- A.mongoSearch uid
  case macc of
    Just acc -> return acc
    Nothing -> do
      userData <- makeUserData
      let acc = getAccount userData
      runConduitCrawl (doRegistration userData)
      mongoSet uid acc
      return acc

doSchedule :: (MonadFree CrawlF cr) => Account -> Maybe Attendance -> Day -> ConduitM () Void cr [Slot]
doSchedule acc attendance today = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  -- a non empty basket is needed for getting schedule
  -- _ <- lift . fromF $ addToBasket "141418" 1
  _ <- lift . fromF $ B.load
  _ <- lift . fromF $ S.load
  slotInfo <- lift . fromF $ S.getSchedule
  return $ map (makeSlot attendance today) slotInfo 




fetchSchedule :: IO ([Slot], UTCTime)
fetchSchedule = do
  now <- getCurrentTime
  attendance <- Att.mongoFind "balma"
  acc <- getAccountFromEnv
  slots <- runConduitCrawl $ doSchedule acc attendance $ utctDay now
  return (slots, addUTCTime (60*5) now)

prepareOrder :: Text -> IO (Either Text DriveBasket)
prepareOrder uid = do
  acc <- doRegisterIfNeeded uid
  mBasket <- CB.mongoFind uid
  mSlotId <- mongoFindChoosenSlot uid
  case mBasket of
    Nothing -> return $ Left "Basket not found"
    Just basket ->
      case mSlotId of
        Nothing -> return $ Left "Slot not found"
        Just slotId -> do
          b <- runConduitCrawl (doPrepareOrder acc basket slotId)
          return $ Right b

-- prepareOrder :: Text -> IO DriveBasket
order :: Text -> IO ()
order uid = do
  acc <- doRegisterIfNeeded uid
  return ()
  -- mt <- GT.mongoFind uid tid
  -- case mt of
  --   Nothing -> putStrLn ("Error: no transaction found" :: Text)
  --   Just t -> do
  --     macc <- A.mongoSearch uid
  --     case macc of
  --       Nothing -> putStrLn ("Error: user not found" :: Text)
  --       Just acc -> do
  --         -- runConduitCrawl $ doTransaction4 acc t
  --         GT.mongoSet uid tid GT.Done
  -- return ()
