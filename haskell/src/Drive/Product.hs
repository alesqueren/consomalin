module Drive.Product (Product(..)
                     , summarize 
                     , mongoInsert 
                     , mongoFind
                     , mongoSearch 
                     ) where

import           Protolude                    hiding (Product, (<>))
import           Database.MongoDB
import           Text.PrettyPrint.Leijen.Text
import           Data.Aeson 
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

-- Mongo

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
  -- TODO: improve
  cast' (Doc doc) = do
    id <- lookup "_id" doc
    price <- lookup "price" doc
    priceByQuantity <- lookup "priceByQuantity" doc
    name <- lookup "name" doc
    nameShort <- lookup "nameShort" doc
    nameLong <- lookup "nameLong" doc
    imageUrl <- lookup "imageUrl" doc
    let quantity = lookup "quantity" doc
    let quantityUnit = lookup "quantityUnit" doc
    let description = lookup "description" doc
    let benefits = lookup "benefits" doc
    let composition = lookup "composition" doc
    return $ Product id price priceByQuantity name nameShort nameLong imageUrl quantity quantityUnit description benefits composition
  cast' _ = Nothing

mongoInsert :: [Product] -> IO ()
mongoInsert = doInsert ProductResource

mongoFind :: [Text] -> IO [Product]
mongoFind pids =
  doSelect ProductResource (select ["_id" =: ["$in" =: pids]])

mongoSearch :: Text -> IO [Product]
mongoSearch s = 
  doSelect ProductResource (select ["$text" =: ["$search" =: s]])


summarize :: Product -> V.ViewProduct
summarize p =
  V.ViewProduct {
    V.id              = pid p
  , V.name            = name p
  , V.price           = price p
  , V.imageUrl        = imageUrl p
  , V.priceByQuantity = priceByQuantity p
  , V.quantity        = quantity p
  , V.quantityUnit    = quantityUnit p
  }
