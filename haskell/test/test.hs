module Main where

import Protolude
import Test.Tasty

import Drive.ProductTest (productTests)

tests :: TestTree
tests =  testGroup "Tests" [ productTests ]

main :: IO ()
main = defaultMain tests
