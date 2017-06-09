module Drive.Mail.IMAP (Mail(..), fetchUnreadMails) where

import           Protolude hiding (pass)
import           Network.HaskellNet.IMAP
import           Network.HaskellNet.IMAP.SSL
import           Codec.MIME.Parse
import           Codec.MIME.Type
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

import           Utils.Env

data Mail = Mail
  { to :: !Text
  , subject :: !Text
  , body :: Content
  }
  deriving (Show)

getBody :: MIMEValue -> Content
getBody mail =
  case mime_val_content mail of 
    Single b -> b
    Multi _ -> ""

getHeader :: Text -> MIMEValue -> Text
getHeader name mail = 
  fromMaybe "" $ head $ 
    map paramValue $ 
      filter (\x -> paramName x == name) $ 
        mime_val_headers mail

makeMail :: ByteString -> Mail
makeMail bs = 
  Mail 
    (getHeader "to" mime) 
    (getHeader "subject" mime) 
    (getBody mime)
  where mime = parseMIMEMessage $ decodeUtf8 bs

fetchUnreadMails :: IO [Mail]
fetchUnreadMails = do
  host <- T.unpack <$> fromEnvOr "IMAP_HOST" A.takeText "127.0.0.1"
  user <- T.unpack <$> fromEnvOr "IMAP_USER" A.takeText ""
  pass <- T.unpack <$> fromEnvOr "IMAP_PASS" A.takeText ""

  con <- connectIMAPSSLWithSettings host defaultSettingsIMAPSSL
  login con user pass
  select con "INBOX"
  mids <- search con [UNFLAG Seen]
  -- set Seen flag
  msgs <- forM mids (fetch con)
  return $ map makeMail msgs
