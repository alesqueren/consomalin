{-# LANGUAGE DeriveGeneric #-}

module Drive.ConsoBasket (ConsoBasket(..), ConsoProduct(..), mongoFind) where
      
import           Protolude hiding (Product, product)
import           Database.MongoDB
import           Drive.Bs.Mongo

data ConsoProduct = ConsoProduct
  { id :: !Text
  , quantity :: Int
  }
  deriving (Typeable, Show, Eq, Generic)

newtype ConsoBasket = ConsoBasket [ConsoProduct]
  deriving (Typeable, Show, Eq, Generic)

extractPd :: Document -> Maybe ConsoProduct
extractPd doc = do
    i <- lookup "pid" doc
    q <- lookup "quantity" doc
    return $ ConsoProduct i q

instance Val ConsoBasket where
  -- TODO: complete function
  -- val b = val [ "totalPrice" =: totalPrice b ]

  -- | takes a UserResource Document
  cast' (Doc doc) = do
    c <- lookup "currentBasket" doc
    s <- lookup "selectedWishes" c :: Maybe Document
    let pds = mapMaybe extractPd $ chunkAtDepth s 2
    return $ ConsoBasket pds

mongoFind :: Text -> IO (Maybe ConsoBasket)
mongoFind uid =
  doSelectOne UserResource ["_id" =: uid]
