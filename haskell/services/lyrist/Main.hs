module Main where

import Protolude hiding (list)

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL
import Codec.MIME.Parse
import Codec.MIME.Type

import Drive.Mail.Smtp
import Drive.Crawl.Auchan.Page.Mail

smtpServer = "mail.consomalin.ovh"
imapServer = "mail.consomalin.ovh"
username = "antoine@mail.consomalin.ovh"
password = "toto"

processMail :: ByteString -> IO ()
processMail bs = do

  when (subject == "Auchan Drive : confirmation de commande") $
    case parse body of
      Just mailInfos -> sendConfirmation "antoine.lesqueren.perso@gmail.com" mailInfos
      Nothing -> putStrLn ("parsing error" :: Text)

  where 
    mail = parseMIMEMessage $ decodeUtf8 bs
    Single body = mime_val_content $ mail
    headers = mime_val_headers mail
    subject = fromMaybe "" $ head $ map paramValue $ filter (\x -> paramName x == "subject") headers

main :: IO ()
main = do
  putStrLn ("start" :: Text)

  con <- connectIMAPSSLWithSettings imapServer defaultSettingsIMAPSSL
  login con username password
  select con "INBOX"

  -- mids <- search con [UNFLAG Seen]
  mids <- search con [ALLs]

  -- set Seen flag
  forM_ mids (fetch con >=> processMail)

  putStrLn ("end" :: Text)
