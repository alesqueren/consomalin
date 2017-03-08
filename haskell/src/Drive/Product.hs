{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Drive.Product (Product(..)
                     , ProductSummary(..)
                     , Price
                     , toPrice, fromPrice
                     , summarize 
                     , insertProducts 
                     , findProducts 
                     , searchProducts 
                     ) where

import           Numeric
import           Protolude                    hiding (Product, (<>), find, sort)

import           Database.MongoDB
import           Text.PrettyPrint.Leijen.Text

import           Drive.Types
import           Drive.Mongo
import           Data.Aeson 
import           GHC.Generics (Generic)

-- | Price are stored in nanocents
moneyScale :: Rational
moneyScale = 100000000000

newtype Price = Price { nanoCents :: Int64 } deriving (Typeable, Show, Eq, Num, Ord, Generic)

-- | Make a price from a fractional number
toPrice :: RealFrac a => a -> Price
toPrice v = Price $ round (toRational v * moneyScale)

fromPrice :: RealFrac a => Price -> a
fromPrice p = fromRational $ toRational (nanoCents p) / moneyScale

-- Pretty print
instance Pretty Price where
  pretty p = text $ toSL $ showFFloat (Just 2) (fromPrice p :: Double) ""

-- Used to convert to/from a MongoDB value
instance Val Price where
  val   (Price c) = val c
  cast' (Int64 c) = Just $ Price c
  cast' _         = Nothing

data ProductSummary = ProductSummary
  { psId              :: !Text
  , psName            :: !Text
  , psPrice           :: !Price
  , psImageUrl        :: !TextURI
  , psPriceByQuantity :: !Price
  , psquantity        :: Maybe Int64
  , psQuantityUnit    :: Maybe Text
  }
  deriving (Typeable, Eq, Show, Generic)

instance FromJSON Price
instance ToJSON Price where
  toJSON = Number . fromPrice
instance FromJSON ProductSummary
instance ToJSON ProductSummary where
  toJSON ProductSummary{..} = object [
    "name"            .= psName,
    "imageUrl"        .= psImageUrl,
    "price"           .= psPrice,
    "priceByQuantity" .= psPriceByQuantity]

data Product = Product
  { pid             :: !Text
  , price           :: !Price
  , priceByQuantity :: !Price
  , name            :: !Text
  , nameShort       :: !Text
  , nameLong        :: !Text
  , imageUrl        :: !TextURI
  , quantity        :: Maybe Int64
  , quantityUnit    :: Maybe Text
  , description     :: Maybe Text
  , benefits        :: Maybe Text
  , composition     :: Maybe Text
  }
  deriving (Typeable, Show, Eq)

instance Pretty Product where
  pretty p = text "Product" <> encloseSep lbrace rbrace (comma <> space)
                             [ text $ fromStrict (nameShort p)
                             , text (show $ imageUrl p :: LText)
                             , pretty $ price p
                             , pretty $ priceByQuantity p
                             ]

instance Val Product where
  val p = val [ "_id" =: pid p
              , "price" =: val (price p)
              , "priceByQuantity" =: val (priceByQuantity p)
              , "nameShort" =: nameShort p
              , "name" =: name p
              , "nameLong" =: nameLong p
              , "imageUrl" =: val (imageUrl p)
              , "quantity" =: quantity p
              , "quantityUnit" =: quantityUnit p
              , "description" =: description p
              , "benefits" =: benefits p
              , "composition" =: composition p
              ]
  -- TODO: set quantity...
  cast' (Doc doc) = do
    id <- lookup "_id" doc :: Maybe Text
    price <- lookup "price" doc :: Maybe Price
    priceByQuantity <- lookup "priceByQuantity" doc :: Maybe Price
    name <- lookup "name" doc :: Maybe Text
    nameShort <- lookup "nameShort" doc :: Maybe Text
    nameLong <- lookup "nameLong" doc :: Maybe Text
    imageUrl <- lookup "imageUrl" doc :: Maybe TextURI
    -- quantity <- lookup "quantity" doc :: Maybe Int64
    -- quantityUnit <- lookup "quantityUnit" doc :: Maybe Text
    -- description <- lookup "description" doc :: Maybe Text
    -- benefits <- lookup "benefits" doc :: Maybe Text
    -- composition <- lookup "composition" doc :: Maybe Text
    -- return $ Product id price priceByQuantity nameShort nameLong imageUrl (Just quantity) (Just quantityUnit) (Just description) (Just benefits) (Just composition)
    return $ Product id price priceByQuantity name nameShort nameLong imageUrl Nothing Nothing Nothing Nothing Nothing
  cast' _ = Nothing

summarize :: Product -> ProductSummary
summarize p =
  ProductSummary {
    psId              = pid p
  , psName            = nameShort p
  , psPrice           = price p
  , psImageUrl        = imageUrl p
  , psPriceByQuantity = priceByQuantity p
  , psquantity        = quantity p
  , psQuantityUnit    = quantityUnit p
  }


-- Mongo

insertProducts :: [Product] -> IO ()
insertProducts = doInsert ProductResource

findProducts :: [Text] -> IO [Product]
findProducts pids =
  doSelect ProductResource (select ["_id" =: ["$in" =: pids]])

searchProducts :: Text -> IO [Product]
searchProducts s = 
  doSelect ProductResource (select ["$text" =: ["$search" =: s]])
