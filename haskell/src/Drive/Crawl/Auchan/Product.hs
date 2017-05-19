module Drive.Crawl.Auchan.Product (SiteProduct(..)
                                  , ApiProduct(..)
                                  , readSiteId 
                                  , makeSiteProduct
                                  , makeSiteProduct2
                                  , makeProduct
                                  ) where

import           Protolude      hiding (Product)
import           Prelude                    (String)
import qualified Data.Text                  as T
import           Data.Map.Strict
import           Text.Regex.TDFA
import           Drive.Types
import           Drive.Price
import           Drive.Product

-- Product info fetched from auchandrive.fr
data SiteProduct = SiteProduct
  { siteId              :: !Text
  , siteName            :: !Text
  , siteImageUrl        :: !TextURI
  , sitePrice           :: !Price
  , sitePriceByQuantity :: !Price
  , siteQuantityUnit    :: !Text
  }
  deriving (Typeable, Show)

-- Product info fetched from merchandising.io
data ApiProduct = ApiProduct
  { apiNameShort    :: !Text
  , apiNameLong     :: !Text
  , apiQuantity     :: Maybe Int64
  , apiDescription  :: Maybe Text
  , apiBenefits     :: Maybe Text
  , apiComposition  :: Maybe Text
  }
  deriving (Typeable, Show, Eq)

readSiteId :: Text -> Maybe Text
readSiteId txt =
  do
    idStr <- head $ getAllTextSubmatches matches
    return $ T.init $ T.drop 2 $ T.pack idStr
  where
    re = "-P([0-9]+)/$" :: String
    matches = T.unpack txt =~ re :: AllTextSubmatches [] String

makeSiteProduct2 :: Map Text Text -> Maybe SiteProduct
makeSiteProduct2 elementsMap =
  do
    [idTxt, priceTxt, nameTxt, imageTxt, priceByQuantityTxt, quantityUnitTxt] 
      <- mapM (`lookup` elementsMap)
        ["id", "name", "image", "price", "priceByQ", "qtyUnit"]
    id <- readSiteId idTxt
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


makeSiteProduct :: Text -> Text -> Text -> Text -> Text -> Text -> Maybe SiteProduct
makeSiteProduct idTxt priceTxt nameTxt imageTxt priceByQuantityTxt quantityUnitTxt =
  do
    id <- readSiteId idTxt
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

makeProduct :: SiteProduct -> Maybe ApiProduct -> Product
makeProduct sitePd apiPd =
  Product
    { pid             = siteId sitePd
    , price           = sitePrice sitePd
    , priceByQuantity = sitePriceByQuantity sitePd
    , name            = siteName sitePd
    , nameShort       = maybe "" apiNameShort apiPd
    , nameLong        = maybe "" apiNameLong apiPd
    , imageUrl        = siteImageUrl sitePd
    , quantity        = maybe Nothing apiQuantity apiPd
    , quantityUnit    = siteQuantityUnit sitePd
    , benefits        = maybe Nothing apiBenefits apiPd
    , description     = maybe Nothing apiDescription apiPd
    , composition     = maybe Nothing apiComposition apiPd
  }
