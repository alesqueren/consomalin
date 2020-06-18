{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Drive.Price (Price
                   , toPrice
                   , fromPrice
                   , readPrice
                   ) where

import           Protolude hiding (from, to)
import           Prelude                    (String, fail)
import           Data.Aeson 
import           Numeric
import           Text.PrettyPrint.Leijen.Text
import           Text.Regex.TDFA
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import           Database.MongoDB

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
  cast' (Float c) = Just $ toPrice c
  cast' _         = Nothing

instance FromJSON Price where
  parseJSON (Number n) = return $ toPrice n
  parseJSON _  = fail "expected a number"
instance ToJSON Price where
  toJSON = Number . fromPrice

readPrice :: Text -> Maybe Price
readPrice txt =
  do
    priceStr <- head $ getAllTextMatches matches
    either
      (const Nothing)
      (\(r,_) -> return $ toPrice (r :: Double))
      (T.rational $ T.pack priceStr)
  where
    newTxt = T.unpack $ foldr (\(from, to) t -> T.replace from to t) txt mapping
    mapping = [(",", "."), (" ", ""), ("\n", ""), ("\t", ""), ("\8364", "")]
    re = "[0-9]+.[0-9][0-9]" :: String
    matches = newTxt =~ re :: AllTextMatches [] String
