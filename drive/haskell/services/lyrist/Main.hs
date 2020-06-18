module Main where

import Protolude hiding (list)
import qualified System.IO as SIO
import qualified Data.Text as T

import Drive.Mail.IMAP as IMAP
import Drive.Mail.SMTP as SMTP
import Drive.Mail.Confirmation
import Drive.Crawl.Auchan.Page.Mail
import Drive.Bs.Rabbitmq

processMail :: IMAP.Mail -> IO ()
processMail inputMail = do

  when (subj == "Auchan Drive : confirmation de commande") $
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

  when (isPrefixOf "=?ISO-8859-1?Q?Annulation_de_la_commande_n=B0" $ T.unpack subj) $ do
    putText "Send cancel message"
    send CancelResource $ CancelMessage $
      T.replace "=?ISO-8859-1?Q?Annulation_de_la_commande_n=B0" "" $
      T.replace "?=" "" subj

  where subj = IMAP.subject inputMail

main :: IO ()
main = do
  SIO.hSetBuffering stdout SIO.NoBuffering
  putText "Fetch unread mails..."
  mails <- fetchUnreadMails
  putText $ "Found " <> show (length mails) <> " unread messages."

  mapM_ processMail mails
