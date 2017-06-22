{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Payment (validatePayment) where

import           Protolude       hiding (Selector)
import           Drive.Crawl

-- Requirement: Schedule.confirmSlot add been called
validatePayment :: Crawl ()
validatePayment = do
  $(logDebug) "validate payment"
  _ <- requestTag $ Req url "POST" hdrs httpData
  return ()
    where
      url = "https://www.auchandrive.fr/drive/paiementauretrait.withdrawalpayment.formvalidate"
      hdrs = [("X-Requested-With", "XMLHttpRequest")
             , ("Referer", "https://www.auchandrive.fr/drive/paiementauretrait")
             , ("User-Agent", "curl/7.53.1" )
             , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")]
      -- TODO: get from $("div.pageAvantages-conditions input[name='t:formdata']").attr("value") 
      --       statically loaded with https://www.auchandrive.fr/drive/paiementauretrait
      httpData = "t%3Aformdata=JtI6n25tDCe4XUT9Q9NqLIIM5J0%3D%3AH4sIAAAAAAAAAC2NIQ7CQBAADxJA4EBiEDjCVWHAgEM0pKEOt1w37ZL27rK3pe0L%2BAwP4FP8gZKgRkwm8%2FqoUbNR6wQIK7RyrC8oDCS7hqTIGBooPXQ%2FpWtLhqQLrA6Ocw0eTIFawGMQ7rbaOMaSbj0r72xfBH2iLEO7StgZDCGtbxWFQM5en8t5u3iPh2oQq6lxVtiVZ6hQ1Cy%2BwwOiEmwepcJk833rRU3%2B9y%2BC%2FiFRsQAAAA%3D%3D&unicity=0.426279360142352&t%3Azoneid=zonePaiement"
