module Utils.Misc (module Utils.Misc) where

import           Protolude
import           Control.Monad.Catch

takeWhileM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (a:as) =
  do v <- a
     if p v
      then do vs <- takeWhileM p as
              return (v:vs)
      else return []

maybeOrThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeOrThrow e Nothing  = throwM e
maybeOrThrow _ (Just r) = return r
