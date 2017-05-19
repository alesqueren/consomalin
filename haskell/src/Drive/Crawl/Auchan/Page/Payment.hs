module Drive.Crawl.Auchan.Page.Payment (doValidatePayment) where

import           Protolude       hiding (Selector)
import           Text.HTML.TagSoup
import           Drive.Crawl

load :: Crawl [Tag Text] 
load = requestTag $ Req url "GET" [] ""
  where url = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.chooseslot"

doValidatePayment :: Crawl ()
doValidatePayment = do
  _ <- load
  _ <- requestTag $ Req url "GET" hdrs httpData
  return ()
    where
      url = "https://www.auchandrive.fr/drive/paiementauretrait.withdrawalpayment.formvalidate"
      hdrs = [("X-Requested-With", "XMLHttpRequest")
                , ("Referer", "https://www.auchandrive.fr/drive/paiementauretrait")
                , ("User-Agent", "curl/7.53.1" )
                , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")]
      httpData = "t%3Aformdata=H4sIAAAAAAAAAC2NIQ7CQBAADxJA4EBiEDjCVWHAgEM0pKEOt1w37ZL27rK3pe0L%2BAwP4FP8gZKgRkwm8%2FqoUbNR6wQIK7RyrC8oDCS7hqTIGBooPXQ%2FpWtLhqQLrA6Ocw0eTIFawGMQ7rbaOMaSbj0r72xfBH2iLEO7StgZDCGtbxWFQM5en8t5u3iPh2oQq6lxVtiVZ6hQ1Cy%2BwwOiEmwepcJk833rRU3%2B9y%2BC%2FiFRsQAAAA%3D%3D&unicity=0.6927108574186207&t%3Azoneid=zonePaiement"
