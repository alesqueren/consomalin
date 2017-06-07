module Main where

import Protolude hiding (list)

-- import Network.HaskellNet.SSL
-- import Network.HaskellNet.IMAP
-- import Network.HaskellNet.IMAP.SSL

import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL

smtpServer = "mail.consomalin.ovh"
imapServer = "mail.consomalin.ovh"
username = "antoine@mail.consomalin.ovh"
password = "toto"

mailto = "antoine.lesqueren.perso@gmail.com"
mailfrom = "antoine@mail.consomalin.ovh"

-- imapCfg :: Settings
-- imapCfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }

processMail :: ByteString -> IO ()
processMail bs =
  print bs

main :: IO ()
main = do
  putStrLn ("start" :: Text)

  -- con <- connectIMAPSSLWithSettings imapServer imapCfg
  -- login con username password
  -- select con "INBOX"

  -- mids <- search con [UNFLAG Seen]
  -- putStrLn (show mids :: Text)

  -- -- set Seen flag
  -- forM_ mids (fetch con >=> processMail)

  -- doSMTPPort smtpServer 587 $ \conn -> do
  --   putStrLn ("1" :: Text)
  --   sendPlainTextMail mailto mailfrom "subj" "COUOU!" conn

  doSMTPSTARTTLS smtpServer $ \conn -> do
    authSucceed <- authenticate LOGIN username password conn
    if authSucceed
      then sendPlainTextMail mailto mailfrom "subj" "COUOU!" conn
      else print "auth failed"

  putStrLn ("end" :: Text)
