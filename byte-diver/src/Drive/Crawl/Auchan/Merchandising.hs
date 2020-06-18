{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Merchandising (fetchProductAuchanData) where

import Protolude hiding (Product)
import Drive.Types
import           Drive.Crawl hiding (html)
import           Drive.Product
import           Drive.Crawl.Auchan.Category
import qualified Data.Text           as T
import           Data.Aeson 
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Strict (lookup)


data MerchData = MerchData
  { cdId           :: !Text
  , cdNameShort    :: !Text
  , cdNameLong     :: !Text
  , cdImageUrl     :: !TextURI
  , cdQuantity     :: Maybe Int64
  , cdQuantityUnit :: Maybe Text
  , cdDescription  :: Maybe Text
  , cdBenefits     :: Maybe Text
  , cdComposition  :: Maybe Text
  }
  deriving (Typeable, Show, Eq)

data Measure = Measure { name :: Text }
  deriving (Show, Generic)
data DataField = DataField 
  { namePublicShort :: Text 
  , namePublicLong  :: Text 
  , quantityNormalized :: Maybe Int64
  , isMeasuredBy :: Maybe Measure
  , description :: Maybe Text
  , productBenefits :: Maybe Text
  , composition :: Maybe Text
  }
  deriving (Show, Generic)
instance FromJSON Measure
instance ToJSON Measure
instance FromJSON DataField
instance ToJSON DataField

parseMerchData :: Text -> Maybe DataField
parseMerchData resp = do
  jsonResponse <- decode' $ BL.fromStrict $ encodeUtf8 resp :: Maybe Object
  d <- lookup "data" jsonResponse 
  decode' $ encode d :: Maybe DataField

fetchMerchData :: Text -> Text -> MerchData
fetchMerchData id html =
    case parseMerchData html of
      Nothing -> 
        MerchData
          { cdId           = id
          , cdNameShort    = ""
          , cdNameLong     = ""
          , cdImageUrl     = newImage
          , cdQuantity     = Nothing
          , cdQuantityUnit = Nothing
          , cdDescription  = Nothing
          , cdBenefits     = Nothing
          , cdComposition  = Nothing
          }
      Just resp ->
        MerchData
          { cdId           = id
          , cdNameShort    = namePublicShort resp
          , cdNameLong     = namePublicLong resp
          , cdImageUrl     = newImage
          , cdQuantity     = quantityNormalized resp
          , cdQuantityUnit = Nothing -- name $ isMeasuredBy resp
          , cdDescription  = Drive.Crawl.Auchan.Merchandising.description resp
          , cdBenefits     = productBenefits resp
          , cdComposition  = Drive.Crawl.Auchan.Merchandising.composition resp
          }
  where 
    image = "http://www.auchandrive.fr/drive/static-media/front/pictures/product/zoom/{pid}.jpg"
    newImage = T.replace "{pid}" id image

getMerchUrl :: Text -> Text
getMerchUrl id = T.replace "{pid}" id url
    where url = "http://merch.productpage.io/merch/v1/productinshop/product?productinshop_shortidout={pid}&productinshop_shopid=1&with_issubstitutablewith=1&with_iscomplementarywith=1"

makeProduct :: MerchData -> AuchanData -> Product
makeProduct cd ad =
  Product
    { pid             = cdId cd
    , price           = adPrice ad
    , priceByQuantity = adPriceByQuantity ad
    , Drive.Product.name = adName ad
    , nameShort       = cdNameShort cd
    , nameLong        = cdNameLong cd
    , imageUrl        = adImageUrl ad
    , quantity        = cdQuantity cd
    , quantityUnit    = Just $ adQuantityUnit ad
    -- , quantityUnit    = fromMaybe (cdQuantityUnit cd) (adQuantityUnit ad)
    , benefits        = cdBenefits cd
    , Drive.Product.description = cdDescription cd
    , Drive.Product.composition = cdComposition cd
  }

fetchProductAuchanData :: Crawl cr => AuchanData -> cr Product
fetchProductAuchanData ad =
  do
    goURI $ getMerchUrl (adId ad)
    html <- getHtml []
    let cd = fetchMerchData (adId ad) html
    return $ makeProduct cd ad
