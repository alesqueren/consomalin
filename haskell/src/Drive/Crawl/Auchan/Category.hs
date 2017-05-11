{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Category (fetchAuchanData) where

import           Protolude                  hiding (Selector)
import           Prelude                    (String)
import           Data.Aeson
import           Data.List                  (nubBy)
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BL
import           Text.HTML.TagSoup
import           Text.Regex.TDFA

import           Drive.Crawl                hiding (html)
import           Drive.Price
import           Drive.Crawl.Auchan.Product

newtype Zone = Zone { itemsList :: Text }
  deriving (Show, Generic)
instance FromJSON Zone

newtype PaginationJSON = PaginationJSON { zones :: Zone }
  deriving (Show, Generic)
instance FromJSON PaginationJSON

getCatUrl :: Integer -> Text
getCatUrl pageNb = "http://www.auchandrive.fr/drive/rayon.productlist.pagination_0.topage/"
                <> show pageNb
                <> "?t:ac=3686969/3686339"

getHtmlCategoryPage :: Text -> Maybe [Tag Text]
getHtmlCategoryPage html = do
  jsonResponse <- decode' $ BL.fromStrict $ encodeUtf8 html
  return $ parseTags $ itemsList $ zones jsonResponse

fetchAuchanDataFromPageNb :: Text -> [Maybe SiteProduct]
fetchAuchanDataFromPageNb html =
  maybe [] identity $ scrape entryProducts tags
  where tags = maybe [] identity $ getHtmlCategoryPage html


extractId :: Text -> Maybe Text
extractId txt =
  do
    idStr <- head $ getAllTextSubmatches matches
    return $ T.init $ T.drop 2 $ T.pack idStr
  where
    re = "-P([0-9]+)/$" :: String
    matches = T.unpack txt =~ re :: AllTextSubmatches [] String

entryProducts :: Scraper Text [Maybe SiteProduct]
entryProducts = chroots productDivSel (productInfo anySelector)

-- TODO: merge with productInfo
buildAuchanData :: Text -> Text -> Text -> Text -> Text -> Text -> Maybe SiteProduct
buildAuchanData idTxt priceTxt nameTxt imageTxt priceByQuantityTxt quantityUnitTxt =
  do
    id <- extractId idTxt
    pr <- readPrice priceTxt
    prByQ <- readPrice priceByQuantityTxt
    return SiteProduct {
      siteId = id,
      siteName = nameTxt,
      siteImageUrl = T.append "http://www.auchandrive.fr" imageTxt,
      sitePrice = pr,
      sitePriceByQuantity = prByQ,
      siteQuantityUnit = quantityUnitTxt
    }

productInfo :: Selector -> Scraper Text (Maybe SiteProduct)
productInfo _ = do
  idTxt <- attr "href" $ "a" @: [hasClass "seoActionLink"]
  nameTxt <- text $ "p" @: [hasClass "libelle-produit"]
  imageTxt <- attr "data-src" $ "div" @: [hasClass "visuel-produit"] // "img"
  priceTxt <- text $ "p" @: [hasClass "prix-produit"]
  priceByQuantityTxt <- text $ "p" @: [hasClass "prix-unitaire"]
  quantityUnitTxt <- text $ "p" @: [hasClass "prix-unitaire"] // "abbr"

  return $ buildAuchanData idTxt priceTxt nameTxt imageTxt priceByQuantityTxt quantityUnitTxt

productDivSel :: Selector
productDivSel = "div" @: [hasClass "vignette",
                          notP $ hasClass "vignette-indispo"]
             // "div" @: [hasClass "vignette-content"]

parseCategoryPage :: Integer -> Crawl [Maybe SiteProduct]
parseCategoryPage pageNb =
  do
    resp <- postText (getCatUrl pageNb) [("X-Requested-With", "XMLHttpRequest")] ""
    return $ fetchAuchanDataFromPageNb resp

fetchAuchanData :: Text -> Crawl [SiteProduct]
fetchAuchanData url =
  do
    goURI url
    _ <- getText "" []  -- FIXME: still needed?
    hotByPage <- takeWhileM (not . null) (map parseCategoryPage [1,2..])
    return $ nubBy (\x y -> siteId x == siteId y) $ catMaybes $ concat hotByPage
