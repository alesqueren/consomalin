{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Register (UserData(..), makeUserData, register, getAccount) where

-- import           Prelude
import           Protolude                  hiding (Selector)
import           Text.HTML.TagSoup
import           Generics.SOP
import           Network.HTTP.Types.Status (statusCode)
import           Control.Monad.Catch
import           System.Random
import qualified GHC.Generics as G
import qualified Data.Text as T
import           Data.Random
import           Data.Random.Extras (choice)
import qualified Data.ByteString.Lazy as LB

import           Drive.Crawl
import           Drive.Crawl.Account

data RegistrationException = RegistrationException deriving (Show, Typeable)
instance Exception RegistrationException


data UserData = UserData
  { email1 :: Text
  , email2 :: Text
  , motDePasse1 :: Text
  , motDePasse2 :: Text
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

randomLine :: Text -> IO Text
randomLine filepath = do
  filecontents <- readFile $ T.unpack filepath
  sample $ choice $ T.lines filecontents

randomWord :: IO Text
randomWord = do
  randString <- mapM genChar ([0..16] :: [Integer])
  return (T.pack randString)
    where genChar _ = randomRIO ('a', 'z') :: IO Char

randomPhone :: IO Text
randomPhone = do
  phone <- mapM (\x -> getStdRandom (randomR (1, 9))) ([0..7] :: [Integer]) :: IO [Integer]
  return $ "08" <> foldl (\x y -> x <> show y) "" phone

-- TODO: generate random fields
makeUserData :: IO UserData
makeUserData = do
  n <- randomLine "./resources/userInfo/familyNames.txt"
  p <- randomLine "./resources/userInfo/firstNames.txt"
  -- city <- randomLine "./resources/cities.txt"
  password <- randomWord
  let email = T.replace " " "" $ T.toLower n <> "." <> T.toLower p <> "@gmail.com"
  phone <- randomPhone
  birthM <- randomRIO (1, 12) :: IO Integer
  birthD <- randomRIO (10, 28) :: IO Integer
  birthY <- randomRIO (1950, 1998) :: IO Integer
  return UserData 
    { email1 = email
    , email2 = email
    , motDePasse1 = "A" <> password <> "666!"
    , motDePasse2 = "A" <> password <> "666!"
    , civilite = "MR"
    , organisme = "2"
    , raisonSociale = ""
    , nom = n
    , prenom = p
    , jourNaissance = show birthD
    , moisNaissance = show birthM
    , anneeNaissance = show birthY
    , telephoneMobile = phone
    , telephoneFixe = ""
    , telephonePro = ""
    , hidden_0 = "6"
    , pays = "France"
    , codePostal = "31500"
    , ville = "TOULOUSE"
    , voie = "DU 10 AVRIL"
    , typeVoie = "RUE"
    , numeroVoie1 = "3"
    , numeroVoie2 = ""
    , etage = ""
    , escalier = ""
    , batiment = ""
    }

-- TODO: use generic
mkFormData :: UserData -> Text
mkFormData u = 
  "email1" <> "=" <> email1 u <> "&" <>
  "email2" <> "=" <> email2 u <> "&" <>
  "motDePasse1" <> "=" <> motDePasse1 u <> "&" <>
  "motDePasse2" <> "=" <> motDePasse2 u <> "&" <>
  "civilite" <> "=" <> civilite u <> "&" <>
  "organisme" <> "=" <> organisme u <> "&" <>
  "raisonSociale" <> "=" <> raisonSociale u <> "&" <>
  "nom" <> "=" <> nom u <> "&" <>
  "prenom" <> "=" <> prenom u <> "&" <>
  "jourNaissance" <> "=" <> jourNaissance u <> "&" <>
  "moisNaissance" <> "=" <> moisNaissance u <> "&" <>
  "anneeNaissance" <> "=" <> anneeNaissance u <> "&" <>
  "telephoneMobile" <> "=" <> telephoneMobile u <> "&" <>
  "telephoneFixe" <> "=" <> telephoneFixe u <> "&" <>
  "telephonePro" <> "=" <> telephonePro u <> "&" <>
  "hidden_0" <> "=" <> hidden_0 u <> "&" <>
  "pays" <> "=" <> pays u <> "&" <>
  "codePostal" <> "=" <> codePostal u <> "&" <>
  "ville" <> "=" <> ville u <> "&" <>
  "voie" <> "=" <> voie u <> "&" <>
  "typeVoie" <> "=" <> typeVoie u <> "&" <>
  "numeroVoie1" <> "=" <> numeroVoie1 u <> "&" <>
  "numeroVoie2" <> "=" <> numeroVoie2 u <> "&" <>
  "etage" <> "=" <> etage u <> "&" <>
  "escalier" <> "=" <> escalier u <> "&" <>
  "batiment" <> "=" <> batiment u <> "&" <>
  "t%3Asubmit=%5B%22submit_1%22%2C%22submit_0%22%5D&" <>
  "t%3Azoneid=formRefresh&" <>
  "t%3Aformdata=bYTU8hopA0BAtWR43AIc8mC0gL8%3D%3AH4sIAAAAAAAAAKVXO28TMRx3Q1sF%2BkBqxYBYGGCivSTXpC2FgaqoUKlUEREMLJVzcVNXd7axnSZdmJCYGVn4BIiBh8TUgQ5IHfgOfAAWkJhAwj737lKloLOz2b%2B73%2BP8%2FN%2Bb72Cs64NyEGJEZGmDiIBjJjElKzhrBzRilKgXvADv4xBLJDhYprztQQaDXeRJyJCQ%2FKDmBZSjEDe9lCK8h7CF6T1OO%2BxaA8kOe1k%2BnHs%2F%2FfNzAYxsgomAEslpuAUjJMHM5h7ch6UQknapITkm7Vs9JkExsXWMWnWIWjnuHR%2B%2Fa%2Fw5LADQY90FUMlrrMwgwSJC4il4BoAE51PESUarjHWXQC0vk0MsKBE0wDDUn1%2F77%2Bc3oUDealOBMJDrGIUtM03XHx1NfLv05ffANOk8I3papoxRwxgNk3DVNmGd0wAJ0eg0IyyE0j5626ru%2FHr91czWPLiRNwqhUTxPRQnOqbYlVTMnu2Xg5eUwjvocx03XXsD4Woz4Hu1wokZdQBKgxH5Ko1sJ6ixnHSaiWAyG0ahTmFNyJswyWMzLhoQgNJBmOoazOM6CJs9NsJSXLlGI2K5qR7SJwzTQxRR%2FEONDSFrPV8rfwb1svlJ0XaHOcibMIqhasxmnSZbJFFQHg6uYSWJxw%2BziVguR7bI6wO7kvWHux5zB8%2BvJi6uzvSufxv992hYTu64H5nKfFvBAJIM0qju2ZDMoVeDnvnZpCzEqJAwT3wsaqseQm5DJUALzeanq4s82zljcs6YbU4vR2qc49RzVHVuy9fqTBwz1uxY18Fg7O4gY9xpYyH37dSLEqaZWkgATBtMRKo5SQ8Twz4jhO0pZLzkkYTtbcnHPmm69AJAIoHqVpwsgAVxErN2bUOJIH2qJewK4iFgXUiiCOExX3rjp2gu4%2BfqnfX17Aet1HlGpzkMoRN92U9hdVI8xR6khYvhnxLDabn1S1kUAkhzpS1L9EKS7blKDayegq5hJchus5L%2BmYiKDXOqij6P0vp05eVTPHg0nbL1WhS4vpE15EhckcrA8edX4cfnjh%2BdrBVDYVL%2Fjsf9GK65NVLmiiii9h0%2BXK8Z8u5I1y38B8Hnggm4QAAA%3D"

getAccount :: UserData -> Account
getAccount u = Account "" (email1 u) (motDePasse1 u)

load :: Crawl [Tag Text]
load = requestTag $ Req url "GET" [] ""
  where url = "https://www.auchandrive.fr/drive/client/inscription"

register :: UserData -> Crawl ()
register u = do
  $(logDebug) $ "register " <> show u
  _ <- load
  res <- request $ Req url "POST" hdrs formData

  -- the response body must be small (no html errors inside)
  when (statusCode (responseStatus res) /= 200 || 
        LB.length (responseBody res) > 100) $ 
    throwM RegistrationException

  return ()
    where 
      url = "https://www.auchandrive.fr/drive/client/inscription.inscriptioncomponent.forminscription"
      hdrs = [("User-Agent", "curl/7.53.1" )
               , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
               , ("X-Requested-With", "XMLHttpRequest")
               ]
      formData = mkFormData u
