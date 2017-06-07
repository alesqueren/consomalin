{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Drive.Basket (Transaction(..), Basket(..), BasketProduct(..), 
  MBasket(..), MBasketProduct(..), diffBasket) where

import           Protolude hiding (Product, product)
import           Data.Aeson
import qualified Data.List as L
import qualified Data.Map.Lazy as M

import           Drive.Price

data BasketProduct = BasketProduct
  { priceByProduct :: Price
  , productNb :: Int
  , price :: Price
  }
  deriving (Typeable, Show, Eq, Generic)

data Basket = Basket
  { totalPrice :: Price
  , products :: Map Text BasketProduct
  }
  deriving (Typeable, Show, Eq, Generic)

data Transaction = Transaction
  { slotId :: Text
  , basket :: Basket
  }
  deriving (Typeable, Show, Eq, Generic)


data MBasketProduct = MBasketProduct
  { mPriceByProduct :: Maybe Price
  , mProductNb :: Maybe Int
  , mPrice :: Maybe Price
  }
  deriving (Typeable, Show, Eq, Generic)

data MBasket = MBasket
  { mTotalPrice :: Maybe Price
  , mProducts :: Map Text MBasketProduct
  }
  deriving (Typeable, Show, Eq, Generic)


diffProduct :: Maybe BasketProduct -> Maybe BasketProduct -> Maybe MBasketProduct
diffProduct Nothing Nothing = Nothing
diffProduct (Just BasketProduct{..}) Nothing = 
  return $ MBasketProduct (Just priceByProduct) (Just productNb) (Just price)
diffProduct Nothing (Just BasketProduct{..}) = 
  return $ MBasketProduct Nothing (Just 0) Nothing
diffProduct (Just b1) (Just b2) =
  if isNothing mUP && isNothing mQ && isNothing mP
     then Nothing 
     else return $ MBasketProduct mUP mQ mP
  where 
    mUP = f (priceByProduct b1) (priceByProduct b2)
    mQ = f (productNb b1) (productNb b2)
    mP = f (price b1) (price b2)
    f v1 v2 = if v1 /= v2 then Just v1 else Nothing

diffProducts :: Map Text BasketProduct -> Map Text BasketProduct -> Map Text MBasketProduct
diffProducts m1 m2 = 
  M.fromList $ mapMaybe process allKeys
    where 
      allKeys = L.nub $ M.keys m1 <> M.keys m2
      process k = case diffProduct (M.lookup k m1) (M.lookup k m2) of
                    Just v -> Just (k, v)
                    Nothing -> Nothing

diffBasket :: Basket -> Basket -> Maybe MBasket
diffBasket b1 b2 =
  if isNothing tp && null mbp 
    then Nothing
    else return $ MBasket tp mbp
    where 
      tp = if totalPrice b1 == totalPrice b2
              then Nothing
              else Just (totalPrice b1)
      mbp = diffProducts (products b1) (products b2)


-- JSON
instance FromJSON Transaction
instance ToJSON Transaction

instance FromJSON Basket
instance ToJSON Basket

instance FromJSON BasketProduct
instance ToJSON BasketProduct

instance FromJSON MBasket
instance ToJSON MBasket where
  toJSON MBasket{..} = 
    object $ catMaybes
      [ toAesonIfJust mTotalPrice "totalPrice" 
      , if null mProducts
          then Nothing
          else Just $ "products" .= mProducts ]
    where 
      toAesonIfJust Nothing _ = Nothing
      toAesonIfJust value name = Just $ name .= value

-- toAesonIfJust :: Maybe t -> Text -> Maybe Pair
-- toAesonIfJust Nothing _ = Nothing
-- toAesonIfJust value name = Just $ name .= value

instance FromJSON MBasketProduct
instance ToJSON MBasketProduct where
  toJSON MBasketProduct{..} = 
    object $ catMaybes 
      [ toAesonIfJust mPriceByProduct "priceByProduct"
      , toAesonIfJust mProductNb "productNb"
      , toAesonIfJust mPrice "price"
      ]
    where 
      toAesonIfJust Nothing _ = Nothing
      toAesonIfJust value name = Just $ name .= value
