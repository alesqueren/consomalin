{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Basket (load, addToBasket, getBasket, emptyBasket) where

import           Prelude         (read)
import           Protolude       hiding (Selector, Product)
import           Control.Monad.Catch
import           Network.HTTP.Types.Status (statusCode)
import           Text.HTML.TagSoup
import qualified Data.Text                  as T
import qualified Data.Map.Strict as M

import           Drive.Price
import           Utils.Misc
import           Drive.Crawl
import           Drive.DriveBasket
import           Drive.ConsoBasket
import           Drive.Crawl.Auchan.Product

data AddToBasketException = AddToBasketException deriving (Show, Typeable)
instance Exception AddToBasketException 

data EmptyBasketException = EmptyBasketException deriving (Show, Typeable)
instance Exception EmptyBasketException

data ParseBasketException = ParseBasketException deriving (Show, Typeable)
instance Exception ParseBasketException


lineScraper :: EntityScraper DriveProduct
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

addToBasket :: ConsoProduct -> Crawl ()
addToBasket (ConsoProduct id qty) = do
  $(logDebug) ("A2B" <> id)

  res <- request $ Req url "POST" hdr httpData

  -- TODO: try to catch "unkown product exception"
  when ((statusCode . responseStatus $ res) /= 200) $
    throwM AddToBasketException

  when (qty /= 1) $ 
    addToBasket $ ConsoProduct id (qty-1)

  return ()
  where
    url = "http://www.auchandrive.fr/drive/productdetail.product.product_addproducttobasket2/" 
          <> id <> "/1/product_addToBasketZone?t:ac=" <> id
    hdr = [("X-Requested-With", "XMLHttpRequest")]
    httpData = "t%3Azoneid=forceAjax"

emptyBasket :: Crawl ()
emptyBasket = do
  $(logDebug) "emptyBasket"

  _ <- load
  res <- request $ Req url "POST" hdr httpData

  -- TODO: try to catch "unkown product exception"
  when ((statusCode . responseStatus $ res) /= 200) $
    throwM EmptyBasketException

  return ()
  where
    url = "https://www.auchandrive.fr/drive/coffre.pagetemplate.popuphandler.popinremovebasket.supprimer"
    hdr = [("X-Requested-With", "XMLHttpRequest")]
    httpData = "t%3Azoneid=forceAjax"

makeLine :: Map Text Text -> Maybe DriveProduct
makeLine elementsMap = do
  [pidTxt, qty, unitPriceTxt, tot] 
    <- mapM (`M.lookup` elementsMap)
      ["pid", "quantity", "unitPrice", "totalPrice"]
  pid <- readSiteId pidTxt
  up <- readPrice unitPriceTxt
  pr <- readPrice tot
  return $ DriveProduct pid up (read $ T.unpack qty) pr

getBasket :: Crawl DriveBasket
getBasket = do
  $(logDebug) "get basket"
  page <- load
  pr <- maybeOrThrow ParseBasketException $ join $ head $ entityScrap totalPriceScraper page
  let pds = catMaybes $ entityScrap lineScraper page
  return $ DriveBasket pr pds 
