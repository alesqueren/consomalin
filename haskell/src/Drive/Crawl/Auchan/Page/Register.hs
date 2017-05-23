{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Register (register) where

import           Protolude                  hiding (Selector)
import           Text.HTML.TagSoup
import           Generics.SOP
import qualified GHC.Generics as G

import           Drive.Crawl
import           Utils.Generic

load :: Crawl [Tag Text]
load = requestTag $ Req url "" [] ""
  where url = "https://www.auchandrive.fr/drive/client/inscription"

data UserData = UserData
  { email :: Text
  , motDePasse :: Text
  , civilite :: Text
  , organisme :: Text
  , raisonSociale :: Text
  , nom :: Text
  , prenom :: Text
  , jourNaissance :: Text
  , moisNaissance :: Text
  , anneeNaissance :: Text
  , telephoneMobile :: Text
  , telephoneFixe :: Text
  , telephonePro :: Text
  , hidden_0 :: Text
  , pays :: Text
  , codePostal :: Text
  , ville :: Text
  , voie :: Text
  , typeVoie :: Text
  , numeroVoie1 :: Text
  , numeroVoie2 :: Text
  , etage :: Text
  , escalier :: Text
  , batiment :: Text
  } deriving (Show, G.Generic)

instance Generics.SOP.Generic UserData

getUserData :: UserData
getUserData = UserData 
  { email = "goto.devnull@mailoo.org"
  , motDePasse = "Toto2222"
  , civilite = "MR"
  , organisme = "2"
  , raisonSociale = ""
  , nom = ""
  , prenom = ""
  , jourNaissance = "06"
  , moisNaissance = "06"
  , anneeNaissance = "1986"
  , telephoneMobile = "0666666666"
  , telephoneFixe = ""
  , telephonePro = ""
  , hidden_0 = "6"
  , pays = "France"
  , codePostal = "31500"
  , ville = "TOULOUSE"
  , voie = "DU 10 AVRIL"
  , typeVoie = "RUE"
  , numeroVoie1 = "4"
  , numeroVoie2 = ""
  , etage = ""
  , escalier = ""
  , batiment = ""
  }

mkFormData :: UserData -> Text
mkFormData user = foldl (<>) "" $ stringValues user

register :: Crawl ()
register = do
  $(logDebug) (mkFormData getUserData)
  return ()
