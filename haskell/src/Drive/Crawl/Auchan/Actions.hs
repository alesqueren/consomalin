{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveGeneric #-}

-- TODO: RM THIS FILE
module Drive.Crawl.Auchan.Actions (doTransaction, doSchedule) where

import           Protolude       hiding (Selector)
import           Control.Monad.Trans.Free.Church
import           Conduit
import           Data.Time

import           Drive.Slot
import           Drive.Transaction
import           Drive.Attendance
import           Drive.Crawl
import           Drive.Crawl.Account
import           Drive.Crawl.Auchan.Schedule
import           Drive.Crawl.Auchan.Page.Landing as L
import           Drive.Crawl.Auchan.Page.Basket as B
import           Drive.Crawl.Auchan.Page.Login as Lo

-- Schedule page
goSchedule :: Crawl ()
goSchedule = do
  $(logDebug) ""
  $(logDebug) "goSchedule"
  _ <- getText url []

  return ()
    where
      url = "https://www.auchandrive.fr/drive/coffre.basketsummary.finalisercoffre"

-- Payment page
goPayment :: Crawl ()
goPayment = do
  $(logDebug) ""
  $(logDebug) "goPayment"
  _ <- getText url []

  return ()
    where
      url = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.chooseslot"

validatePayment :: Crawl ()
validatePayment = do
  $(logDebug) ""
  $(logDebug) "validatePayment"
  _ <- postText url headers httpData

  return ()
    where
      url = "https://www.auchandrive.fr/drive/paiementauretrait.withdrawalpayment.formvalidate"
      headers = [("X-Requested-With", "XMLHttpRequest")
                , ("Referer", "https://www.auchandrive.fr/drive/paiementauretrait")
                , ("User-Agent", "curl/7.53.1" )
                , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")]
      httpData = "t%3Aformdata=H4sIAAAAAAAAAC2NIQ7CQBAADxJA4EBiEDjCVWHAgEM0pKEOt1w37ZL27rK3pe0L%2BAwP4FP8gZKgRkwm8%2FqoUbNR6wQIK7RyrC8oDCS7hqTIGBooPXQ%2FpWtLhqQLrA6Ocw0eTIFawGMQ7rbaOMaSbj0r72xfBH2iLEO7StgZDCGtbxWFQM5en8t5u3iPh2oQq6lxVtiVZ6hQ1Cy%2BwwOiEmwepcJk833rRU3%2B9y%2BC%2FiFRsQAAAA%3D%3D&unicity=0.6927108574186207&t%3Azoneid=zonePaiement"


-- TODO: mv to Journey.hs (?)
doTransaction :: (MonadFree CrawlF cr) => Account -> Transaction -> ConduitM () Void cr ()
doTransaction acc t = do

  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  _ <- mapM (lift . fromF . addToBasket) $ basket t
  _ <- lift . fromF $ B.load
  _ <- lift . fromF $ goSchedule
  _ <- lift . fromF $ selectSchedule $ slot t
  _ <- lift . fromF $ goPayment
  -- _ <- lift . fromF $ validatePayment

  return ()

-- TODO: return Slot and not SlotInfo
doSchedule :: (MonadFree CrawlF cr) => Account -> Attendance -> Day -> ConduitM () Void cr [Slot]
doSchedule acc attendance today = do
  _ <- lift . fromF $ doChooseDrive "Toulouse-954"
  _ <- lift . fromF $ Lo.login acc
  -- TODO: make sure to add an existing product
  _ <- lift . fromF $ addToBasket ("141418", 1)
  -- a non empty basket is needed for getting schedule
  _ <- lift . fromF $ B.load
  _ <- lift . fromF $ goSchedule
  lift . fromF $ getSchedule attendance today 
