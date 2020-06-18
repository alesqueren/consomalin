{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Cancellation (cancelTransaction) where

import           Protolude       hiding (Selector)
import           Control.Monad.Catch
import           Network.HTTP.Types.Status (statusCode)

import           Drive.Crawl

data LoadException = LoadException deriving (Show, Typeable)
instance Exception LoadException

loadProfile :: Crawl (Response LByteString)
loadProfile = request $ Req url "GET" [] ""
  where url = "https://www.auchandrive.fr/drive/client/moncompte"

loadCancelForm :: Text -> Crawl (Response LByteString)
loadCancelForm tid = do
  page <- request $ Req url "GET" [] ""
  when ((statusCode . responseStatus $ page) /= 200) $
    throwM LoadException
  return page
  where url = "https://www.auchandrive.fr/drive/client/contact/annulercommande/" <> tid

validateCancelForm :: Text -> Text -> Crawl (Response LByteString)
validateCancelForm uid tid = do
  p <- request $ Req url "POST" hdr httpData
  when ((statusCode . responseStatus $ p) /= 200) $
    throwM LoadException
  return p
  where
    url = "https://www.auchandrive.fr/drive/client/contact/annulercommande.formcontact"
    hdr = [("User-Agent", "curl/7.53.1" )
          , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
          , ("X-Requested-With", "XMLHttpRequest")
          ]
    httpData = "t%3Aac=" <> tid <> "&t%3Aformdata=hyrB%2Fl%2BB%2BJaCeg5DE1bmJ%2FkJcsM%3D%3AH4sIAAAAAAAAAJ2TwUrDMBjHs4Gim%2FMg%2BAaKzs1WYQPR01DnZYhYfYC0zbZImsQk3erFd%2FAFfALxoPcd3EHYwXfwATwJnhRMWiuI4FZPCT%2F4%2Fr9%2FPtqbFzDVL4MVj2BEle0xqqCn7AalIUFilwUBpD7a9nAPE6yQFGCLiY4FOfS6yFKQI6nERd3ymEAEu%2FoMOKM6SlrH0MfsQLCQLzlIhfxqY1C9n399yINcCxSNSTByCAOkwELrDPagTSDt2I4SmHZ2Iq7ATKrN1rD2j4abo2g0unM%2BBnkAIj6Bj3FEqS6vffU%2FfS6UyGq4GuqYJkbET9axfDosPi8%2Bvv9axzm4BLn4%2BcZhSLY6jax1jgTzkJRO6AZYSszo8Navtd%2Bun5JdWKA6gbyNhVRxA9N%2FWoGSoc2UZk0xIYX%2BGlidYAgFEJNUO2vIviFZphNdBZTHfWVdhiNf4N73Mwsx2jMo03xiHL8V7%2BvyQ1pKaeLNmJKo10FlzBANA4UI4l39u6TmOQ1PUpgxI%2FZ%2BAsxyXfRxBAAA&civilite=3&openName=TRINTIGNANT&openFirstname=NATHANAEL&openEmail=" <> uid <> "&choixDrive=954&commandeDrive=" <> tid <> "&numTelephone=0894375266&submitForm=false&t%3Azoneid=cr_contact_mainconteneur"


confirmCancelPopup :: Text -> Crawl (Response LByteString)
confirmCancelPopup tid = do
  page <- request $ Req url "POST" hdr httpData
  when ((statusCode . responseStatus $ page) /= 200) $
    throwM LoadException
  return page
  where
    url = "https://www.auchandrive.fr/drive/client/contact/annulercommande.pagetemplate.popuphandler.popinconfirmationcancelordersentmail.closepopup?t:ac=" <> tid
    httpData = "t%3Azoneid=closePopupZone"
    hdr = [("User-Agent", "curl/7.53.1" )
          , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
          , ("X-Requested-With", "XMLHttpRequest")
          ]

cancelTransaction :: Text -> Text -> Crawl ()
cancelTransaction uid tid = do
  $(logDebug) $ "cancel transation " <> uid <> " " <> tid
  _ <- loadProfile
  _ <- loadCancelForm tid
  _ <- validateCancelForm uid tid
  _ <- confirmCancelPopup tid
  return ()
