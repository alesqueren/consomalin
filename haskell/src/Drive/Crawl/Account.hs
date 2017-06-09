module Drive.Crawl.Account (Account(..), 
                            getAccountFromEnv, 
                            mongoSearchById,
                            mongoSearchByMail, 
                            mongoSet) where

import           Protolude hiding (pass)
import qualified Data.Attoparsec.Text as A
import           Database.MongoDB

import           Utils.Env
import           Drive.Bs.Mongo

data Account = Account 
  { uid :: Text
  , user :: Text
  , pass :: Text
  }
  deriving (Typeable, Eq, Show)

instance Val Account where
  val acc = val [ "driveUser" =: user acc
              , "drivePassword" =: pass acc ]
  cast' (Doc doc) = do
    uid <- lookup "_id" doc
    u <- lookup "driveUser" doc
    p <- lookup "drivePassword" doc
    return $ Account uid u p
  cast' _ = Nothing

getAccountFromEnv :: IO Account
getAccountFromEnv = do
  u <- fromEnvOr "DRIVE_USER" A.takeText ""
  p <- fromEnvOr "DRIVE_PASS" A.takeText ""
  return $ Account "" u p

mongoSet :: Account -> IO ()
mongoSet (Account uid u p) = 
  doModify AccountResource [(sel, doc, [Upsert])]
  where 
    sel = [ "_id" =: uid ]
    doc = [ "$set" =: [ "driveUser" =: u, "drivePassword" =: p ] ]

mongoSearchById :: Text -> IO (Maybe Account)
mongoSearchById uid =
  doSelectOne AccountResource ["_id" =: uid]

mongoSearchByMail :: Text -> IO (Maybe Account)
mongoSearchByMail mail =
  doSelectOne AccountResource ["driveUser" =: mail]
