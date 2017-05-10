module Utils.Env (module Drive.Env) where

import           Protolude
import           System.Environment
import           Data.Attoparsec.Text

fromEnv :: Text -> Parser a -> IO (Maybe a)
fromEnv envVar parser = do
  mp <- (lookupEnv . T.unpack) envVar

  return $ do
    v <- fmap T.pack mp
    either (const Nothing) Just $ parseOnly parser v


fromEnvOr :: Text -> Parser a -> a -> IO a
fromEnvOr envVar parser defaultValue = do
  m <- fromEnv envVar parser
  return $ fromMaybe defaultValue m
