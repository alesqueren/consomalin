{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Category (AuchanData(..), fetchAuchanData) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           Drive.Crawl                hiding (html)
import           Drive.Product
import           Drive.Utils
import           Protolude                  hiding (Selector)
import           Text.HTML.TagSoup

import           Data.List                  (nubBy)
import qualified Data.Text.Read             as T
import           Network.HTTP.Types.Header
import           Prelude                    (String)
import           Text.Regex.TDFA

data ProductsNotFoundException = ProductsNotFoundException deriving (Show, Typeable)
instance Exception ProductsNotFoundException

data AuchanData = AuchanData
  { adId              :: !Text
  , adName            :: !Text
  , adImageUrl        :: !TextURI
  , adPrice           :: !Price
  , adPriceByQuantity :: !Price
  , adQuantityUnit    :: !Text
  }
  deriving (Typeable, Show)

newtype Zone = Zone { itemsList :: Text }
  deriving (Show, Generic)
instance FromJSON Zone
instance ToJSON Zone

newtype PaginationJSON = PaginationJSON { zones :: Zone }
  deriving (Show, Generic)
instance FromJSON PaginationJSON
instance ToJSON PaginationJSON

getCatUrl :: Integer -> Text
getCatUrl pageNb = "http://www.auchandrive.fr/drive/rayon.productlist.pagination_0.topage/"
                <> show pageNb
                <> "?t:ac=3686969/3686339"

getHtmlCategoryPage :: Text -> Maybe [Tag Text]
getHtmlCategoryPage html = do
  jsonResponse <- decode' $ BL.fromStrict $ encodeUtf8 html
  return $ parseTags $ itemsList $ zones jsonResponse

fetchAuchanDataFromPageNb :: Text -> [Maybe AuchanData]
fetchAuchanDataFromPageNb html =
  maybe [] identity $ scrape entryProducts tags
  where tags = maybe [] identity $ getHtmlCategoryPage html

extractPrice :: Text -> Maybe Price
extractPrice txt =
  do
    priceStr <- head $ getAllTextMatches matches
    either
      (const Nothing)
      (\(r,_) -> return $ toPrice (r :: Double))
      (T.rational $ T.pack priceStr)
  where
    newTxt = T.unpack $ T.replace "," "." txt
    re = "[0-9]+.[0-9][0-9]" :: String
    matches = newTxt =~ re :: AllTextMatches [] String

extractId :: Text -> Maybe Text
extractId txt =
  do
    idStr <- head $ getAllTextSubmatches matches
    return $ T.init $ T.drop 2 $ T.pack idStr
  where
    re = "-P([0-9]+)/$" :: String
    matches = T.unpack txt =~ re :: AllTextSubmatches [] String

entryProducts :: Scraper Text [Maybe AuchanData]
entryProducts = chroots productDivSel (productInfo anySelector)

buildAuchanData :: Text -> Text -> Text -> Text -> Text -> Text -> Maybe AuchanData
buildAuchanData idTxt priceTxt nameTxt imageTxt priceByQuantityTxt quantityUnitTxt =
  do
    id <- extractId idTxt
    pr <- extractPrice priceTxt
    prByQ <- extractPrice priceByQuantityTxt
    return AuchanData {
      adId = id,
      adName = nameTxt,
      adImageUrl = T.append "http://www.auchandrive.fr" imageTxt,
      adPrice = pr,
      adPriceByQuantity = prByQ,
      adQuantityUnit = quantityUnitTxt
    }

productInfo :: Selector -> Scraper Text (Maybe AuchanData)
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

parseCategoryPage :: Crawl cr => Integer -> cr [Maybe AuchanData]
parseCategoryPage pageNb =
  do
    goURI $ getCatUrl pageNb
    resp <- getHtml [("X-Requested-With" :: HeaderName, "XMLHttpRequest" :: ByteString)]
    return $ fetchAuchanDataFromPageNb resp

fetchAuchanData :: Crawl cr => Text -> cr [AuchanData]
fetchAuchanData url =
  do
    goURI url
    _ <- getHtml []
    hotByPage <- takeWhileM (not . null) (map parseCategoryPage [1,2..])
    return $ nubBy (\x y -> adId x == adId y) $ catMaybes $ concat hotByPage

-- case head $ getAllTextSubmatches matches of
--   Nothing -> return Nothing
--   Just id -> do
--     priceTxt <- text $ "p" @: [hasClass "prix-produit"]
--     return $ Just AuchanData {
--       adPid = T.init $ T.pack $ drop 2 id, -- not pretty
--       adPrice = extractPrice priceTxt
--     }
