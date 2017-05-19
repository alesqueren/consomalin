{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Basket (load, addToBasket, getBasket) where

import           Prelude         (read)
import           Protolude       hiding (Selector)
import           Control.Monad.Catch
import           Network.HTTP.Types.Status (statusCode)
import           Text.HTML.TagSoup
import qualified Data.Text                  as T
import qualified Data.Map.Strict as M

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

lineScraper :: EntityScraper Line
lineScraper = EntityScraper 
  { rootSelector = "li" @: [hasClass "productInfo"]
  , elementSelectors = M.fromList
    [ ("pid", attr "href" $ "div" @: [hasClass "productInfo-imageContent"] //
                            "a" @: [hasClass "seoActionLink"])
    , ("quantity", attr "value" $ "input" @: [hasClass "productInfo-changeQteNumber"])
    , ("unitPrice", text $ "div" @: [hasClass "productInfo-prixUnitContent"] //
                           "div" @: [hasClass "productInfo-prixUnit"])
    , ("totalPrice", text $ "div" @: [hasClass "productInfo-prixTotalContent"] //
                            "div" @: [hasClass "productInfo-prixUnit"])
    ]
  , entityMaker = makeLine
  }

totalPriceScraper :: EntityScraper Price
totalPriceScraper = EntityScraper
  { rootSelector = "body"
  , elementSelectors = M.fromList [ 
    ( "total", text $ "div" @: [hasClass "recapInfos"] //
                      "div" @: [hasClass "recapInfos-prixContent"] //
                      "div" @: [hasClass "recapInfos-prix"])
    ]
  , entityMaker = readPrice . fromMaybe "" . M.lookup "total"
  }

load :: Crawl [Tag Text]
load = requestTag $ Req "https://www.auchandrive.fr/drive/coffre" "GET" [] ""

addToBasket :: Text -> Int64 -> Crawl ()
addToBasket pid qty = do
  $(logDebug) "A2B"

  res <- request $ Req url "POST" hdr httpData

  -- TODO: try to catch "unkown product exception"
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

makeLine :: Map Text Text -> Maybe Line
makeLine elementsMap = do
  [pidTxt, qty, unitPriceTxt, tot] 
    <- mapM (`M.lookup` elementsMap)
      ["pid", "quantity", "unitPrice", "totalPrice"]
  pid <- readSiteId pidTxt
  up <- readPrice unitPriceTxt
  price <- readPrice tot
  return Line
    { productId = pid
    , unitPrice = up
    , quantity = read $ T.unpack qty
    , totalPrice = price
    }

getBasket :: Crawl Basket
getBasket = do
  $(logDebug) "get basket"
  page <- load
  totPrice <- maybeOrThrow ParseBasketException $ join $ head $ entityScrap totalPriceScraper page
  return Basket 
    { lines = catMaybes $ entityScrap lineScraper page
    , total = totPrice
    }
