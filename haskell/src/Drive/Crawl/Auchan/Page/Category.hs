{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Page.Category (loadAndExtract, extractProducts) where

import           Protolude                  hiding (Selector)
import           Data.Aeson
import           Data.List                  (nubBy)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import           Text.HTML.TagSoup

import           Drive.Crawl                hiding (html)
import           Drive.Crawl.Auchan.Product

newtype Zone = Zone { itemsList :: Text }
  deriving (Show, Generic)
instance FromJSON Zone

newtype Page = Page { zones :: Zone }
  deriving (Show, Generic)
instance FromJSON Page

siteProductScraper :: EntityScraper SiteProduct
siteProductScraper = EntityScraper 
  { rootSelector = "div" @: [hasClass "vignette", notP $ hasClass "vignette-indispo"] // 
                   "div" @: [hasClass "vignette-content"]
  , elementSelectors = M.fromList 
      [ ("id", attr "href" $ "a" @: [hasClass "seoActionLink"])
      , ("name", text $ "p" @: [hasClass "libelle-produit"])
      , ("image", attr "data-src" $ "div" @: [hasClass "visuel-produit"] // "img")
      , ("price", text $ "p" @: [hasClass "prix-produit"])
      , ("priceByQ", text $ "p" @: [hasClass "prix-unitaire"])
      , ("qtyUnit", text $ "p" @: [hasClass "prix-unitaire"] // "abbr")
      ]
  , entityMaker = makeSiteProduct
  }

extractProducts :: Page -> [SiteProduct]
extractProducts page =
  nubBy (\x y -> siteId x == siteId y) $ 
    catMaybes $ 
      entityScrap siteProductScraper $
        parseTags . itemsList . zones $
          page

loadPage :: Integer -> Crawl BL.ByteString
loadPage pageNb =
  requestJson $ Req url "POST" [("X-Requested-With", "XMLHttpRequest")] ""
  where url = "http://www.auchandrive.fr/drive/rayon.productlist.pagination_0.topage/"
              <> show pageNb
              <> "?t:ac=3686969/3686339"

load :: Text -> Crawl [Tag Text]
load url = requestTag $ Req url "GET" [] ""

loadAndExtract :: Text -> (Page -> [a]) -> Crawl [a]
loadAndExtract url extractor = 
  do
    -- Set current category in Auchan website state
    _ <- load url

    -- Load and extract every pages of the current category
    itemByPage <- takeWhileM (not . null) (map (extractFromJson extractor . loadPage) [1,2..])
    return $ concat itemByPage
