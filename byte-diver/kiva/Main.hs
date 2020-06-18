module Main where

import           Protolude
import           Drive.Server
import           System.Environment

import           Data.Attoparsec.Text
import qualified Data.Text as T


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


main :: IO ()
main = do
  port <- fromEnvOr "SERVER_PORT" decimal 80 :: IO Int
  startSrv port
