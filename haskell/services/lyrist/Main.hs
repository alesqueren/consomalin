module Main where

import Protolude hiding (list)
import qualified System.IO as SIO

import Drive.Mail.IMAP as IMAP
import Drive.Mail.SMTP as SMTP
import Drive.Mail.Confirmation
import Drive.Crawl.Auchan.Page.Mail

processMail :: IMAP.Mail -> IO ()
processMail inputMail =

  when (IMAP.subject inputMail == "Auchan Drive : confirmation de commande") $
    case parse inputMail of
      Just mailInfos -> do
        outputMail <- makeConsoMail mailInfos
        case outputMail of
          Just outputMail2 -> do
            putText $ "Send confirmation mail to " <> SMTP.to outputMail2
            sendConfirmation outputMail2
          Nothing -> do
            putText $ "No account found for " <> IMAP.to inputMail
            return ()
      Nothing -> putStrLn ("parsing error" :: Text)

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  putText "Fetch unread mails..."
  mails <- fetchUnreadMails
  putText $ "Found " <> show (length mails) <> " unread messages."

  mapM_ processMail mails
