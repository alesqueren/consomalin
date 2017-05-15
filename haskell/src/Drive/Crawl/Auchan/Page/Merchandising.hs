{-# LANGUAGE DeriveGeneric #-}

module Drive.Crawl.Auchan.Page.Merchandising (load, extractApiProduct) where

import Protolude hiding (Product)
import           Drive.Crawl hiding (html)
import           Drive.Crawl.Auchan.Product
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Strict (lookup)

data Page = Page
  { namePublicShort :: Text 
  , namePublicLong  :: Text 
  , quantityNormalized :: Maybe Int64
  , description :: Maybe Text
  , productBenefits :: Maybe Text
  , composition :: Maybe Text
  }
  deriving (Show, Generic)
instance FromJSON Page

extractApiProduct :: Maybe Page -> Maybe ApiProduct
extractApiProduct mPage =
  do
    page <- mPage
    return ApiProduct
      { apiNameShort    = namePublicShort page
      , apiNameLong     = namePublicLong page
      , apiQuantity     = quantityNormalized page
      , apiDescription  = description page
      , apiBenefits     = productBenefits page
      , apiComposition  = composition page
      }

-- TODO: make real page datastructure
parseJson :: BL.ByteString -> Maybe Page
parseJson resp = do
  jsonResponse <- decode' resp :: Maybe Object
  d <- lookup "data" jsonResponse 
  decode' $ encode d

load :: Text -> Crawl (Maybe Page)
load pid =
  do
    resp <- requestJson $ Req url "GET" [] ""
    return . parseJson $ resp
    where url = "http://merch.productpage.io/merch/v1/productinshop/product?productinshop_shortidout="
                <> pid
                <> "&productinshop_shopid=1&with_issubstitutablewith=1&with_iscomplementarywith=1"
