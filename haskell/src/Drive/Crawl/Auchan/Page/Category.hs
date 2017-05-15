{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Page.Category (loadAndExtract, extractProducts) where

import           Protolude                  hiding (Selector)
import           Data.Aeson
import           Data.List                  (nubBy)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Text.HTML.TagSoup

import           Drive.Crawl                hiding (html)
import           Drive.Crawl.Auchan.Product

newtype Zone = Zone { itemsList :: Text }
  deriving (Show, Generic)
instance FromJSON Zone

newtype Page = Page { zones :: Zone }
  deriving (Show, Generic)
instance FromJSON Page

productInfo :: Selector -> Scraper Text (Maybe SiteProduct)
productInfo _ = do
  idTxt <- attr "href" $ "a" @: [hasClass "seoActionLink"]
  nameTxt <- text $ "p" @: [hasClass "libelle-produit"]
  imageTxt <- attr "data-src" $ "div" @: [hasClass "visuel-produit"] // "img"
  priceTxt <- text $ "p" @: [hasClass "prix-produit"]
  priceByQuantityTxt <- text $ "p" @: [hasClass "prix-unitaire"]
  quantityUnitTxt <- text $ "p" @: [hasClass "prix-unitaire"] // "abbr"

  return $ makeSiteProduct idTxt priceTxt nameTxt imageTxt priceByQuantityTxt quantityUnitTxt

productSelector :: Selector
productSelector = "div" @: [hasClass "vignette",
                          notP $ hasClass "vignette-indispo"]
             // "div" @: [hasClass "vignette-content"]

entryProducts :: Scraper Text [Maybe SiteProduct]
entryProducts = chroots productSelector (productInfo anySelector)

extractProducts :: Page -> [SiteProduct]
extractProducts page =
  nubBy (\x y -> siteId x == siteId y) $ 
    catMaybes $ 
      fromMaybe [] $ 
        scrape entryProducts $
          parseTags . itemsList . zones $
            page

extract :: (Page -> [a]) -> Crawl BL.ByteString -> Crawl [a]
extract f page =
  do
    p <- page
    return $ maybe [] f $ decode' p

load :: Integer -> Crawl BL.ByteString
load pageNb =
  requestJson $ Req url "POST" [("X-Requested-With", "XMLHttpRequest")] ""
  where url = "http://www.auchandrive.fr/drive/rayon.productlist.pagination_0.topage/"
              <> show pageNb
              <> "?t:ac=3686969/3686339"

loadAndExtract :: Text -> (Page -> [a]) -> Crawl [a]
loadAndExtract url f = 
  do
    -- Set current category in Auchan website state
    _ <- requestTag $ Req url "GET" [] ""

    -- Load and extract every pages of the current category
    itemByPage <- takeWhileM (not . null) (map (extract f . load) [1,2..])
    return . concat $ itemByPage
