{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Mail.Smtp (MailInfo(..), sendConfirmation) where

import Protolude hiding (from)
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL
import Data.Text as T

-- TODO: env
smtpServer = "mail.consomalin.ovh"
username = "antoine@mail.consomalin.ovh"
password = "toto"

from = "no-reply@mail.consomalin.ovh"
bd = "<html><head></head><body><h1>Hello <i>world!</i></h1></body></html>" 

data MailInfo = MailInfo 
  { clientNb :: !Text
  , transactionNb :: !Text
  , barcode :: !Text
  }
  deriving (Show)

makeBody :: MailInfo -> Text
makeBody MailInfo{..}  = 
  "<img src=" <> barcode <> ">"

sendConfirmation :: Text -> MailInfo -> IO ()
sendConfirmation to info = do

  doSMTPSTARTTLS smtpServer $ \conn -> do
    authSucceed <- authenticate LOGIN username password conn
    if authSucceed
      then sendMimeMail (T.unpack to) from subject "" (fromStrict body) [] conn
      else print "auth failed"

  where 
    subject = "Consomalin: confirmation de commande"
    body = makeBody info
