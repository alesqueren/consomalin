module Drive.Crawl.Account (Account(..), makeAccount) where

import Protolude
import Drive.Utils
import qualified Data.Attoparsec.Text as A

data Account = Account 
  { driveUser :: Text
  , drivePass :: Text
  }
  deriving (Typeable, Eq, Show)

makeAccount :: IO Account
makeAccount = do
  u <- fromEnvOr "DRIVE_USER" A.takeText ""
  p <- fromEnvOr "DRIVE_PASS" A.takeText ""
  return $ Account u p
