{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Drive.Mail.SMTP (Mail(..), sendConfirmation) where

import           Protolude hiding (from, to, pass)
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Data.Text as T
import qualified Data.Attoparsec.Text as A

import           Utils.Env

data Mail = Mail 
  { from :: !Text
  , to :: !Text
  , subject :: !Text
  , body :: !Text
  }

sendConfirmation :: Mail -> IO ()
sendConfirmation Mail{..} = do

  host <- fromEnvOr "SMTP_HOST" A.takeText "127.0.0.1"
  user <- fromEnvOr "SMTP_USER" A.takeText ""
  pass <- fromEnvOr "SMTP_PASS" A.takeText ""

  doSMTPSTARTTLS (T.unpack host) $ \conn -> do
    authSucceed <- authenticate LOGIN (T.unpack user) (T.unpack pass) conn
    if authSucceed
      then
        sendMimeMail (T.unpack to) (T.unpack from) (T.unpack subject) "" (fromStrict body) [] conn
      else 
        putText "SMTP authentification failed"
