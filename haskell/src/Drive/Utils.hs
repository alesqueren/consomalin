module Drive.Utils (module Drive.Utils) where

import           Protolude
import           Control.Monad.Catch
import qualified Data.Text as T
import           Data.Attoparsec.Text
import           System.Environment

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

-- FIXME: move somewhere else?
fromEnv :: Text -> Parser a -> IO (Maybe a)
fromEnv envVar p = do
  mp <- (lookupEnv . T.unpack) envVar

  return $ do
    v <- fmap T.pack mp
    either (const Nothing) Just $ parseOnly p v


fromEnvOr :: Text -> Parser a -> a -> IO a
fromEnvOr envVar p defaultValue = do
  m <- fromEnv envVar p
  return $ fromMaybe defaultValue m
