{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Basket (load, addToBasket, getBasket) where

import           Prelude         (read)
import           Protolude       hiding (Selector)
import           Control.Monad.Catch
import           Network.HTTP.Types.Status (statusCode)
import           Text.HTML.TagSoup
import qualified Data.Text                  as T

import           Drive.Price
import           Utils.Misc
import           Drive.Crawl
import           Drive.Crawl.Auchan.Product

data AddToBasketException = AddToBasketException deriving (Show, Typeable)
instance Exception AddToBasketException

data ParseBasketException = ParseBasketException deriving (Show, Typeable)
instance Exception ParseBasketException

data Line = Line 
  { productId :: Text
  , unitPrice :: Price
  , quantity :: Int
  , totalPrice :: Price
  }
  deriving (Typeable, Show, Eq)

data Basket = Basket 
  { lines :: [Line]
  , total :: Price
  }
  deriving (Typeable, Show, Eq)

data SelectorType = TotalPriceS | LineS | LinePidS | LineQuantityS | LineUnitPriceS | LineTotalPriceS
getSel :: SelectorType -> Selector
getSel TotalPriceS     = "div" @: [hasClass "recapInfos"] //
                         "div" @: [hasClass "recapInfos-prixContent"] //
                         "div" @: [hasClass "recapInfos-prix"]
getSel LineS           = "li" @: [hasClass "productInfo"]
getSel LinePidS        = "div" @: [hasClass "productInfo-imageContent"] //
                         "a" @: [hasClass "seoActionLink"]
getSel LineQuantityS   = "input" @: [hasClass "productInfo-changeQteNumber"]
getSel LineUnitPriceS  = "div" @: [hasClass "productInfo-prixUnitContent"] //
                         "div" @: [hasClass "productInfo-prixUnit"]
getSel LineTotalPriceS = "div" @: [hasClass "productInfo-prixTotalContent"] //
                         "div" @: [hasClass "productInfo-prixUnit"]


load :: Crawl [Tag Text]
load = requestTag $ Req "https://www.auchandrive.fr/drive/coffre" "GET" [] ""

addToBasket :: Text -> Int64 -> Crawl ()
addToBasket pid qty = do
  $(logDebug) "A2B"

  res <- request $ Req url "POST" hdr httpData

  when ((statusCode . responseStatus $ res) /= 200) $
    throwM AddToBasketException

  when (qty /= 1) $ 
    addToBasket pid (qty-1)

  return ()
  where
    url = "http://www.auchandrive.fr/drive/productdetail.product.product_addproducttobasket2/" 
          <> pid <> "/1/product_addToBasketZone?t:ac=" <> pid
    hdr = [("X-Requested-With", "XMLHttpRequest")]
    httpData = "t%3Azoneid=forceAjax"

makeLine :: Text -> Text -> Text -> Text -> Maybe Line
makeLine pidTxt unitPriceTxt qty tot = do
  pid <- readSiteId pidTxt
  up <- readPrice unitPriceTxt
  price <- readPrice tot
  return Line
    { productId = pid
    , unitPrice = up
    , quantity = read $ T.unpack qty
    , totalPrice = price
    }

lineInfo :: Selector -> Scraper Text (Maybe Line)
lineInfo _ = do
  pidTxt <- attr "href" $ getSel LinePidS
  quantityTxt <- attr "value" $ getSel LineQuantityS
  unitPriceTxt <- text $ getSel LineUnitPriceS
  totalPriceTxt <- text $ getSel LineTotalPriceS
  return $ makeLine pidTxt unitPriceTxt quantityTxt totalPriceTxt

entryLines :: Scraper Text [Maybe Line]
entryLines = chroots (getSel LineS) (lineInfo anySelector)

entryTotalPrice :: Scraper Text (Maybe Price)
entryTotalPrice = do
  priceTxt <- text $ getSel TotalPriceS
  return $ readPrice priceTxt

getBasket :: Crawl Basket
getBasket = do
  $(logDebug) "A2B"

  tags <- load

  totPrice <- maybeOrThrow ParseBasketException $ join $ scrape entryTotalPrice tags

  let res = Basket { 
      lines = maybe [] catMaybes $ scrape entryLines tags
      , total = totPrice
      }

  $(logDebug) (show res)
  return res
