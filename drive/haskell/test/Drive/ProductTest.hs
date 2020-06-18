{-# OPTIONS_GHC -fno-warn-orphans #-}
module Drive.ProductTest where

import           Protolude                    hiding (Product)

import qualified Data.Text                    as T
import           Data.Text.Arbitrary          ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Database.MongoDB
import           Text.PrettyPrint.Leijen.Text hiding ((<>))

import           Drive.Product


productTests :: TestTree
productTests = testGroup "Tests for Drive.Product"
  [ unitTests
  , quickChecks
  ]

extractPriceTest :: Text -> TestTree
extractPriceTest t = testCase (T.unpack $ "Read price \"" <> t <> "\"") $
  isJust (extractPrice t) @? "read failed"

prettyPriceTest :: (Rational, Text) -> TestTree
prettyPriceTest (p, t) = testCase (T.unpack $ "Price " <> show (fromRational p :: Double)) $
  (show . pretty) (toPrice p) @?= t

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "extractPrice" $ map extractPriceTest
      [ "1,0", "1.0", "1", "0.01", "1,00",
        "Ça coûte 0.01" ]
  , testGroup "pretty" $ map prettyPriceTest
      ([ (1.16, "1.16")
       , (1.011, "1.01")
       , (0.1, "0.10")
       , (0.0, "0.00")
       ] :: [(Rational, Text)])
  ]

-- | Quickcheck arbitrary instances

instance Arbitrary Price
  where
    arbitrary = fmap toPrice (arbitrary :: Gen Double)

instance Arbitrary Product
  where
    arbitrary = do
      n <- arbitrary
      img <- arbitrary
      pr <- arbitrary
      pbq <- arbitrary
      return $ Product n img pr pbq

quickChecks :: TestTree
quickChecks = testGroup "Quick checks"
  [ testProperty "Price BSON" $
      \p -> (cast' . val) (p :: Price) == Just p
  , testProperty "Product BSON" $
      \p -> (cast' . val) (p :: Product) == Just p
  ]
