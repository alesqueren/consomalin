module Drive.Crawl.Auchan.Product (SiteProduct(..)
                                  , ApiProduct(..)
                                  , makeProduct
                                  ) where

import           Protolude      hiding (Product)
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
  { apiId           :: !Text
  , apiNameShort    :: !Text
  , apiNameLong     :: !Text
  , apiImageUrl     :: !TextURI
  , apiQuantity     :: Maybe Int64
  , apiQuantityUnit :: Maybe Text
  , apiDescription  :: Maybe Text
  , apiBenefits     :: Maybe Text
  , apiComposition  :: Maybe Text
  }
  deriving (Typeable, Show, Eq)

makeProduct :: SiteProduct -> ApiProduct -> Product
makeProduct sitePd apiPd =
  Product
    { pid             = apiId apiPd
    , price           = sitePrice sitePd
    , priceByQuantity = sitePriceByQuantity sitePd
    , Drive.Product.name = siteName sitePd
    , nameShort       = apiNameShort apiPd
    , nameLong        = apiNameLong apiPd
    , imageUrl        = siteImageUrl sitePd
    , quantity        = apiQuantity apiPd
    , quantityUnit    = Just $ siteQuantityUnit sitePd
    -- , quantityUnit    = fromMaybe (apiQuantityUnit apiPd) (siteQuantityUnit sitePd)
    , benefits        = apiBenefits apiPd
    , Drive.Product.description = apiDescription apiPd
    , Drive.Product.composition = apiComposition apiPd
  }
