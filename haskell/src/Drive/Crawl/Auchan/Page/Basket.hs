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
  pidTxt <- attr "href" $ "div" @: [hasClass "productInfo-imageContent"] //
                          "a" @: [hasClass "seoActionLink"]
  quantityTxt <- attr "value" $ "input" @: [hasClass "productInfo-changeQteNumber"]
  unitPriceTxt <- text $ "div" @: [hasClass "productInfo-prixUnitContent"] //
                         "div" @: [hasClass "productInfo-prixUnit"]
  totalPriceTxt <- text $ "div" @: [hasClass "productInfo-prixTotalContent"] //
                          "div" @: [hasClass "productInfo-prixUnit"]
  return $ makeLine pidTxt unitPriceTxt quantityTxt totalPriceTxt

lineSelector :: Selector
lineSelector = "li" @: [hasClass "productInfo"]

entryLines :: Scraper Text [Maybe Line]
entryLines = chroots lineSelector (lineInfo anySelector)


entryTotalPrice :: Scraper Text (Maybe Price)
entryTotalPrice = do
  priceTxt <- text $ "div" @: [hasClass "recapInfos"] //
                     "div" @: [hasClass "recapInfos-prixContent"] //
                     "div" @: [hasClass "recapInfos-prix"]
  return $ readPrice priceTxt


getBasket :: Crawl Basket
getBasket = do
  $(logDebug) "A2B"

  tags <- load

  mtotPrice <- maybeOrThrow ParseBasketException $ scrape entryTotalPrice tags
  totPrice <- maybeOrThrow ParseBasketException mtotPrice

  let res = Basket { 
      lines = maybe [] catMaybes $ scrape entryLines tags
      , total = totPrice
      }

  $(logDebug) (show res)
  return res
