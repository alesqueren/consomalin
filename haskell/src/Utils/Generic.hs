{-# LANGUAGE FlexibleContexts #-}

module Utils.Generic (stringValues, kv) where

import           Prelude
import qualified GHC.Generics as G
import           Generics.SOP
import qualified Data.Text               as T

stringValues :: (Generic a, All2 Show (Code a)) => a -> [T.Text]
stringValues a =
  hcollapse (hcmap (Proxy :: Proxy Show) (\ (I x) -> K (T.pack (show x))) (from a))

kv :: (Generic a, All2 Show (Code a)) => a -> [(Text, Text)]
kv a =
  hcollapse (hcmap (Proxy :: Proxy Show) (\ (I x) -> K (T.pack (show x), T.pack (show x)) (from a))
