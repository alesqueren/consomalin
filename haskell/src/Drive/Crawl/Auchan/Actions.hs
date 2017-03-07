{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Actions (doTransaction0) where

import           Protolude       hiding (Selector)
import           Drive.Crawl
import           Drive.Crawl.Auchan.ShopChoice
import           Control.Monad.Trans.Free.Church
import           Conduit hiding (connect)
import qualified Data.Text       as T

user :: Text
user = "goto.devnull%40mailoo.org"

password :: Text
password = "secret"

connect :: Crawl ()
connect = do
  $(logDebug) ""
  $(logDebug) ""
  $(logDebug) "connect"

  _ <- getText "https://www.auchandrive.fr/drive/client/identification" []
  _ <- postText url header httpData

  return ()
    where
      url = "https://www.auchandrive.fr/drive/client/identification.formidentification"
      httpData = "t%3Aformdata=H4sIAAAAAAAAAFvzloG1XJVBOTknMzWvRN8zBUhmpmUmJ5Zk5udZpeYmZuaUJeZkpiSWpBYXMZjmF6XrJRYkJmek6pUkFqQWlxRVmuol5xel5mQm6SUlFqfqOSYBBROTS9wyU3NSVIJTS0oLVEMPcz8UPf6HiYHRh4E7OT%2BvpCg%2Fxy8xN7WEQcgnK7EsUT8nMS9dP7ikKDMv3bqioISBF2xxGNRi4t3nSKr7Aoryk1OLi4NLk3Izi4uBRh5el2KS9m3eOSYGhoqCcg0GNewWFyQWF5fnF6XA7S5kqGNgKGEQgEnA3U60ESATWMvlGGSwKy8GObEE6EcHvH5Mzs8tyM8D6izWA3uqBNOLM4M%2FSW7d0uLMxMDkw8ABsc0zBWQ9KHpSc1JzgQKg6AELgaKDA2J5vCGCaQAALR8OljkCAAA%3D&emailValidate=" <> user <> "&passwordValidate=" <> password <> "&t%3Asubmit=%5B%22submit_1%22%2C%22submit_0%22%5D&t%3Azoneid=identification"
      header = [("User-Agent", "curl/7.53.1" )
               , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
               , ("X-Requested-With", "XMLHttpRequest")
               ]


addToBasket :: Text -> Crawl ()
addToBasket pid = do
  $(logDebug) ""
  $(logDebug) "add2basket"
  _ <- postText url headers httpData

  return ()
    where
      url = "http://www.auchandrive.fr/drive/rayon.productlist.thumbnailproduct.thumbnailproduct_addproducttobasket2/" <> pid <> "/1/thumbnailProduct_addToBasketZone_" <> pid <> "/-margin_last_30_days_shop/$N/$N/1/$N/$B?t:ac=3686969/3686338"
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"

getBasket :: Crawl ()
getBasket = do
  $(logDebug) ""
  $(logDebug) "getBasket"
  resp <- getText url []

  $(logDebug) (T.pack $ take 50 $ T.unpack resp)

  return ()
    where
      url = "https://www.auchandrive.fr/drive/coffre"

goSchedule :: Crawl ()
goSchedule = do
  $(logDebug) ""
  $(logDebug) "goSchedule"
  _ <- getText url []

  return ()
    where
      url = "https://www.auchandrive.fr/drive/coffre.basketsummary.finalisercoffre"

getDay :: Int -> Crawl ()
getDay day = do
  $(logDebug) ""
  $(logDebug) ("getDay" <> show day)
  _ <- postText url headers httpData

  return ()
    where
      url = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.changejour/" <> show day
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"

selectSchedule :: Text -> Crawl ()
selectSchedule slotId = do
  $(logDebug) ""
  $(logDebug) $ "selectSchedule" <> slotId
  _ <- postText url headers httpData

  return ()
    where
      url = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.selectslot/" <> slotId
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"

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
  resp <- postText url headers httpData

  $(logDebug) (T.pack $ take 50 $ T.unpack resp)

  return ()
    where
      url = "https://www.auchandrive.fr/drive/paiementauretrait.withdrawalpayment.formvalidate"
      headers = [("X-Requested-With", "XMLHttpRequest")
                , ("Referer", "https://www.auchandrive.fr/drive/paiementauretrait")
                , ("User-Agent", "curl/7.53.1" )
                , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")]
      httpData = "t%3Aformdata=H4sIAAAAAAAAAC2NIQ7CQBAADxJA4EBiEDjCVWHAgEM0pKEOt1w37ZL27rK3pe0L%2BAwP4FP8gZKgRkwm8%2FqoUbNR6wQIK7RyrC8oDCS7hqTIGBooPXQ%2FpWtLhqQLrA6Ocw0eTIFawGMQ7rbaOMaSbj0r72xfBH2iLEO7StgZDCGtbxWFQM5en8t5u3iPh2oQq6lxVtiVZ6hQ1Cy%2BwwOiEmwepcJk833rRU3%2B9y%2BC%2FiFRsQAAAA%3D%3D&unicity=0.6927108574186207&t%3Azoneid=zonePaiement"




doTransaction0 :: (MonadFree CrawlF cr) => ConduitM () Void cr ()
doTransaction0 = do

  _ <- lift . fromF $ chooseDrive "Toulouse-954"
  _ <- lift . fromF $ connect
  _ <- lift . fromF $ addToBasket "354342"
  _ <- lift . fromF $ getBasket
  _ <- lift . fromF $ goSchedule
  _ <- lift . fromF $ getDay 3
  _ <- lift . fromF $ selectSchedule "3556569"
  _ <- lift . fromF $ goPayment
  _ <- lift . fromF $ validatePayment

  return ()
