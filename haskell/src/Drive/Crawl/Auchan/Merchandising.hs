{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Merchandising (fetchProductAuchanData) where

import Protolude hiding (Product)
import           Drive.Crawl hiding (html)
import           Drive.Crawl.Auchan.Product
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Strict (lookup)

-- newtype Measure = Measure { name :: Text }
--   deriving (Show, Generic)
-- instance FromJSON Measure
-- instance ToJSON Measure

data DataField = DataField
  { namePublicShort :: Text 
  , namePublicLong  :: Text 
  , quantityNormalized :: Maybe Int64
  -- , isMeasuredBy :: Maybe Measure
  , description :: Maybe Text
  , productBenefits :: Maybe Text
  , composition :: Maybe Text
  }
  deriving (Show, Generic)
instance FromJSON DataField
instance ToJSON DataField

parseMerchData :: Text -> Maybe DataField
parseMerchData resp = do
  jsonResponse <- decode' $ BL.fromStrict $ encodeUtf8 resp :: Maybe Object
  d <- lookup "data" jsonResponse 
  decode' $ encode d :: Maybe DataField

fetchMerchData :: Text -> Text -> ApiProduct
fetchMerchData id html =
    case parseMerchData html of
      Nothing -> 
        ApiProduct
          { apiId           = id
          , apiNameShort    = ""
          , apiNameLong     = ""
          , apiImageUrl     = image
          , apiQuantity     = Nothing
          , apiQuantityUnit = Nothing
          , apiDescription  = Nothing
          , apiBenefits     = Nothing
          , apiComposition  = Nothing
          }
      Just resp ->
        ApiProduct
          { apiId           = id
          , apiNameShort    = namePublicShort resp
          , apiNameLong     = namePublicLong resp
          , apiImageUrl     = image
          , apiQuantity     = quantityNormalized resp
          , apiQuantityUnit = Nothing -- name $ isMeasuredBy resp
          , apiDescription  = Drive.Crawl.Auchan.Merchandising.description resp
          , apiBenefits     = productBenefits resp
          , apiComposition  = Drive.Crawl.Auchan.Merchandising.composition resp
          }
  where 
    image = "http://www.auchandrive.fr/drive/static-media/front/pictures/product/zoom/" <> id <> ".jpg"

getMerchUrl :: Text -> Text
getMerchUrl id = "http://merch.productpage.io/merch/v1/productinshop/product?productinshop_shortidout="
              <> id
              <> "&productinshop_shopid=1&with_issubstitutablewith=1&with_iscomplementarywith=1"

fetchProductAuchanData :: Text -> Crawl ApiProduct
fetchProductAuchanData pid =
  do
    goURI $ getMerchUrl pid
    jsonR <- getText "" []
    return $ fetchMerchData pid jsonR
