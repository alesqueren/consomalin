{-# LANGUAGE DeriveGeneric #-}

module Drive.DriveBasket (DriveBasket(..), DriveProduct(..)) where

import           Protolude hiding (Product, product)

import           Drive.Price

data DriveProduct = DriveProduct
  { id :: !Text
  , unitPrice :: Price
  , quantity :: Int
  , price :: Price
  }
  deriving (Typeable, Show, Eq, Generic)

data DriveBasket = DriveBasket
  { totalPrice :: Price
  , products :: [DriveProduct]
  }
  deriving (Typeable, Show, Eq, Generic)

-- TODO: makeDriveBasket
