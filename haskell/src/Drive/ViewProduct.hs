{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Drive.ViewProduct (ViewProduct(..)) where

import           Protolude
import           Data.Aeson 
import           Drive.Types
import           Drive.Price

-- Minimum fields needed to be displayed to the final user
data ViewProduct= ViewProduct
  { id              :: !Text
  , name            :: !Text
  , price           :: !Price
  , imageUrl        :: !TextURI
  , priceByQuantity :: !Price
  , quantity        :: Maybe Int64
  , quantityUnit    :: Maybe Text
  }
  deriving (Typeable, Eq, Show, Generic)

instance FromJSON ViewProduct
instance ToJSON ViewProduct where
  toJSON ViewProduct{..} = 
    object [ "name"            .= name
           , "imageUrl"        .= imageUrl
           , "price"           .= price
           , "priceByQuantity" .= priceByQuantity
           , "quantityUnit"    .= quantityUnit
           ]
