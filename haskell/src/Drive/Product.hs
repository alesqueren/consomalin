{-# LANGUAGE RecordWildCards #-}

module Drive.Product (Product(..)
                     , summarize 
                     , mongoInsert 
                     , mongoFind
                     , mongoFindOne 
                     , mongoSearch 
                     ) where

import           Protolude                    hiding (Product, (<>))
import           Database.MongoDB
import           Text.PrettyPrint.Leijen.Text
import           Drive.Types
import           Drive.Price
import qualified Drive.ViewProduct as V
import           Drive.Bs.Mongo

data Product = Product
  { pid             :: !Text
  , price           :: !Price
  , priceByQuantity :: !Price
  , name            :: !Text
  , nameShort       :: !Text
  , nameLong        :: !Text
  , imageUrl        :: !TextURI
  , quantityUnit    :: !Text
  , quantity        :: Maybe Int64
  , description     :: Maybe Text
  , benefits        :: Maybe Text
  , composition     :: Maybe Text
  }
  deriving (Typeable, Show, Eq)

instance Pretty Product where
  pretty Product{..} = 
    text "Product" <> 
         encloseSep lbrace rbrace (comma <> space)
         [ text $ fromStrict nameShort
         , text (show imageUrl :: LText)
         , pretty price
         , pretty priceByQuantity
         ]

instance Val Product where
  val Product{..} = 
    val [ "_id" =: pid
        , "price" =: val price
        , "priceByQuantity" =: val priceByQuantity
        , "nameShort" =: nameShort
        , "name" =: name
        , "nameLong" =: nameLong
        , "imageUrl" =: val imageUrl
        , "quantity" =: quantity
        , "quantityUnit" =: quantityUnit
        , "description" =: description
        , "benefits" =: benefits
        , "composition" =: composition
        ]
  -- TODO: improve
  cast' (Doc doc) = do
    id <- lookup "_id" doc
    price <- lookup "price" doc
    priceByQuantity <- lookup "priceByQuantity" doc
    name <- lookup "name" doc
    nameShort <- lookup "nameShort" doc
    nameLong <- lookup "nameLong" doc
    imageUrl <- lookup "imageUrl" doc
    quantityUnit <- lookup "quantityUnit" doc
    let quantity = lookup "quantity" doc
    let description = lookup "description" doc
    let benefits = lookup "benefits" doc
    let composition = lookup "composition" doc
    return $ Product id price priceByQuantity name nameShort nameLong imageUrl quantityUnit quantity description benefits composition
  cast' _ = Nothing

mongoInsert :: [Product] -> IO ()
mongoInsert = doInsert ProductResource

mongoFindOne :: IO (Maybe Product)
mongoFindOne =
  doSelectOne ProductResource []

mongoFind :: [Text] -> IO [Product]
mongoFind pids =
  doSelect ProductResource (select ["_id" =: ["$in" =: pids]])

mongoSearch :: Text -> IO [Product]
mongoSearch s = 
  doSelect ProductResource (select ["$text" =: ["$search" =: s]])


summarize :: Product -> V.ViewProduct
summarize Product{..} =
  V.ViewProduct {
    V.id              = pid
  , V.name            = name
  , V.price           = price
  , V.imageUrl        = imageUrl
  , V.quantityUnit    = quantityUnit
  , V.priceByQuantity = priceByQuantity
  }
