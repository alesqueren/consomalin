{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Basket (load, addToBasket) where

import           Protolude       hiding (Selector)
import           Drive.Crawl

addToBasket :: (Text, Int64) -> Crawl ()
addToBasket (pid, qty) = 
  do
    $(logDebug) "A2B"

    _ <- request $ Req url "POST" hdr httpData
    -- _ <- mapM (const $ postText url hdr httpData) [1..qty]

    return ()
    where
      -- url = "http://www.auchandrive.fr/drive/rayon.productlist.thumbnailproduct.thumbnailproduct_addproducttobasket2/" <> pid <> "/1/thumbnailProduct_addToBasketZone_" <> pid <> "/-margin_last_30_days_shop/$N/$N/1/$N/$B?t:ac=3686969/3686338"
      url = "http://www.auchandrive.fr/drive/productdetail.product.product_addproducttobasket2/" <> pid <> "/1/product_addToBasketZone?t:ac=" <> pid
      hdr = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"

load :: Crawl ()
load = do
  _ <- request $ Req "https://www.auchandrive.fr/drive/coffre" "GET" [] ""
  return ()
