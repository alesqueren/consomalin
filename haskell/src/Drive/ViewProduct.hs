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
  , quantityUnit    :: Text
  , priceByQuantity :: !Price
  }
  deriving (Typeable, Eq, Show, Generic)

instance FromJSON ViewProduct
instance ToJSON ViewProduct where
  toJSON ViewProduct{..} = 
    object [ "name"            .= name
           , "price"           .= price
           , "imageUrl"        .= imageUrl
           , "quantityUnit"    .= quantityUnit
           , "priceByQuantity" .= priceByQuantity
           ]
