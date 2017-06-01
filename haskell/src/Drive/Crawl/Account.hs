module Drive.Crawl.Account (Account(..), 
                            getAccountFromEnv, 
                            mongoSearch,
                            mongoSet) where

import           Protolude hiding (pass)
import qualified Data.Attoparsec.Text as A
import           Database.MongoDB

import           Utils.Env
import           Drive.Bs.Mongo

data Account = Account 
  { user :: Text
  , pass :: Text
  }
  deriving (Typeable, Eq, Show)

instance Val Account where
  -- TODO: complete function
  val a = val [ "driveUser" =: user a
              , "drivePassword" =: pass a ]
  cast' (Doc doc) = do
    u <- lookup "driveUser" doc
    p <- lookup "drivePassword" doc
    return $ Account u p

getAccountFromEnv :: IO Account
getAccountFromEnv = do
  u <- fromEnvOr "DRIVE_USER" A.takeText ""
  p <- fromEnvOr "DRIVE_PASS" A.takeText ""
  return $ Account u p

mongoSet :: Text -> Account -> IO ()
mongoSet uid (Account u p) = 
  doModify UserResource [(sel, doc, [])]
  where 
    sel = [ "_id" =: uid ]
    doc = [ "$set" =: [ "driveUser" =: u, "drivePassword" =: p ] ]

mongoSearch :: Text -> IO (Maybe Account)
mongoSearch uid =
  doSelectOne UserResource ["_id" =: uid]
