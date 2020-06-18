{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Page.Payment (enterPromoCode, validatePayment) where

import           Protolude       hiding (Selector)
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT

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


-- Requirement: Schedule.confirmSlot add been called
enterPromoCode :: Crawl Bool
enterPromoCode = do
  $(logDebug) "enter promoCode"
  page <- request $ Req url "POST" hdrs httpData
  let str = T.unpack $ toStrict $ LT.decodeUtf8 $ responseBody page
  return $ isInfixOf "Votre code avantage a bien été pris en compte." str
    where
      url = "https://www.auchandrive.fr/drive/paiementauretrait.withdrawalpayment.addbr.formbr"
      hdrs = [("X-Requested-With", "XMLHttpRequest")
             , ("Referer", "https://www.auchandrive.fr/drive/paiementauretrait")
             , ("User-Agent", "curl/7.53.1" )
             , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")]
      httpData = "t%3Aformdata=EuSC2%2Bh%2FxSTw5VAVMesUYIvIqms%3D%3AH4sIAAAAAAAAAKXRP0sDMRgG8NeCWqiT4iAuDnW9orYiutgqolCk9HBxey%2BJbeQuicl7Xrs4CX4GFz%2BBOCm4d3DzO%2FgBXBycHLw7HZSCtDgl5N%2Fz48ntK0wma7DSQikioagetwVZlLSZSOpyiwmGBvvZloecB9aLlWSS%2Bs7CtrYdDw2yrvAIjXBk%2BzWPaStCGaRjZLRK7zlvX3IuVLllNRPO%2BXEQSeekVsdXS3O9xcepAkw0ocS0IqvDQ4wEwWzzFM%2BxEqLqVHyyUnW2eoZg%2Bjs92YD10clMcxFYLpWJKXXX%2FnQH6IRXD9JFZLQnRcjLvqDYLB8NSi%2FzTx9D2DO4gIkMN5PlNNq7ec6%2FhPVxhUPVDu549eT95rkA0DNJFVZHt7jsEQrsOD%2BcB9Mw49p%2FW3i4v9wpQKEJRRbK9PQBzxtLSxRhTvpVYvErvtH%2BMf0ELSneAaUCAAA%3D&unicity=0.5744434334688168&codeBRDinput=JEDRIVE&t%3Asubmit=%5B%22submitBR%22%2C%22submitBR%22%5D&t%3Azoneid=forceAjax"
