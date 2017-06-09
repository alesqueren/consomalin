{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Drive.Mail.Confirmation (MailInfo(..), makeConsoMail) where

import Protolude hiding (to)
import Data.Text as T
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet

import Drive.Crawl.Account
import Drive.Mail.SMTP

data MailInfo = MailInfo
  { to :: !Text
  , clientNb :: !Text
  , transactionNb :: !Text
  , barcode :: !Text
  , slot :: !Text
  }
  deriving (Show)

makeConsoMail :: MailInfo -> IO (Maybe Mail)
makeConsoMail MailInfo{..} = do
  macc <- mongoSearchByMail to
  return (do
    (Account uid _ _) <- macc
    return $ Mail "no-reply@mail.consomalin.ovh"
               uid
               "Consomalin: confirmation de commande"
               (T.pack $ renderHtml ( $(shamletFile "./resources/mails/confirmation.hamlet") )))
