{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards #-}

module Drive.Crawl.Auchan (crawl, doRegisterIfNeeded, fetchSchedule, prepareOrder, order, checkBasket, cancelTr, checkPromoCode) where

import Protolude           hiding (Product)
import Conduit
import Control.Monad.Trans.Free.Church
import Data.Time
import qualified Data.Map.Lazy as M

import Drive.Slot
import Drive.Basket as BA
import Drive.Product
import Drive.Attendance as Att
import Drive.Crawl hiding (html)
import Drive.Crawl.Account as A
import Drive.Crawl.Auchan.Product
import Drive.Crawl.Auchan.Page.Home as H
import Drive.Crawl.Auchan.Page.Login as Lo
import Drive.Crawl.Auchan.Page.Orders as O
import Drive.Crawl.Auchan.Page.Basket as B
import Drive.Crawl.Auchan.Page.Landing as L
import Drive.Crawl.Auchan.Page.Payment as P
import Drive.Crawl.Auchan.Page.Register as R
import Drive.Crawl.Auchan.Page.Schedule as S
import Drive.Crawl.Auchan.Page.Category as C
import Drive.Crawl.Auchan.Page.Cancellation as Ca
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
    -- $(logDebug) $ "[AuchanDataPage] " <> show sitePd
    page <- M.load $ siteId sitePd
    let pd = makeProduct sitePd $ extractApiProduct page
    -- $(logDebug) $ show pd
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

doPrepareOrder :: (MonadFree CrawlF cr) => Account -> Transaction -> ConduitM () Void cr (Maybe MBasket)
doPrepareOrder acc Transaction{..} = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  _ <- lift . fromF $ B.emptyBasket
  _ <- mapM (\(pid,qty) -> lift $ fromF $ B.addToBasket pid qty) pds
  driveBasket <- lift . fromF $ B.getBasket
  _ <- lift . fromF $ S.selectSchedule slotId
  return $ diffBasket driveBasket basket
  where
    pds = M.elems $
          M.mapWithKey (\pid pd -> (pid, BA.productNb pd)) $
          BA.products basket

-- TODO: merge with doPrepareOrder
doCheckPromoCode :: (MonadFree CrawlF cr) => Account -> Transaction -> ConduitM () Void cr Bool
doCheckPromoCode acc Transaction{..} = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  a <- lift . fromF $ Lo.login acc
  _ <- lift . fromF $ B.getBasket
  _ <- lift . fromF $ S.selectSchedule slotId
  lift . fromF $ P.enterPromoCode

doCheckBasket  :: (MonadFree CrawlF cr) => Account -> Transaction -> ConduitM () Void cr Basket
doCheckBasket acc Transaction{..} = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  lift . fromF $ B.getBasket 


doOrder :: (MonadFree CrawlF cr) => Account -> Transaction -> ConduitM () Void cr Text
doOrder acc Transaction{..} = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  _ <- lift . fromF $ B.getBasket 
  _ <- lift . fromF $ S.selectSchedule slotId
  _ <- lift . fromF $ P.enterPromoCode
  _ <- lift . fromF $ P.validatePayment
  lift . fromF $ O.getLastTid

doRegisterIfNeeded :: Text -> IO Account
doRegisterIfNeeded uid = do
  macc <- A.mongoSearchById uid
  case macc of
    Just acc -> return acc
    Nothing -> do
      userData <- makeUserData
      let acc = getAccount uid userData
      runConduitCrawl (doRegistration userData)
      mongoSet acc
      return acc

doSchedule :: (MonadFree CrawlF cr) => Account -> Maybe Attendance -> Day -> Text -> ConduitM () Void cr [Slot]
doSchedule acc attendance today pid = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  -- a non empty basket is needed for getting schedule
  _ <- lift . fromF $ addToBasket pid 1
  _ <- lift . fromF $ B.load
  slotInfo <- lift . fromF $ S.getSchedule
  return $ map (makeSlot attendance today) slotInfo 

doCancellation :: (MonadFree CrawlF cr) => Account -> Text -> ConduitM () Void cr ()
doCancellation acc tid = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  _ <- lift . fromF $ O.getLastTid
  _ <- lift . fromF $ Ca.cancelTransaction (user acc) tid
  return ()


fetchSchedule :: IO ([Slot], UTCTime)
fetchSchedule = do
  now <- getCurrentTime
  attendance <- Att.mongoFind "balma"
  acc <- getAccountFromEnv
  pid <- maybe "" pid <$> mongoFindOne
  slots <- runConduitCrawl $ doSchedule acc attendance (utctDay now) pid
  return (slots, addUTCTime (60*5) now)

checkPromoCode :: Text -> Transaction -> IO Bool
checkPromoCode uid transaction = do
  acc <- doRegisterIfNeeded uid
  runConduitCrawl (doCheckPromoCode acc transaction)

prepareOrder :: Text -> Transaction -> IO (Maybe MBasket)
prepareOrder uid transaction = do
  acc <- doRegisterIfNeeded uid
  runConduitCrawl (doPrepareOrder acc transaction)

checkBasket :: Text -> Transaction -> IO (Maybe MBasket)
checkBasket uid transaction = do
  acc <- doRegisterIfNeeded uid
  driveBasket <- runConduitCrawl (doCheckBasket acc transaction)
  return $ diffBasket driveBasket (basket transaction)

order :: Text -> Transaction -> IO Text
order uid transaction = do
  acc <- doRegisterIfNeeded uid
  runConduitCrawl (doOrder acc transaction)

cancelTr :: Text -> Text -> IO ()
cancelTr uid tid = do
  acc <- doRegisterIfNeeded uid
  runConduitCrawl (doCancellation acc tid)
