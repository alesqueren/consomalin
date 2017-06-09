{-# LANGUAGE RecordWildCards #-}

module Drive.Crawl.Auchan.Page.Mail (parse) where

import           Protolude hiding (to)
import           Prelude                    (String)
import           Text.HTML.TagSoup
import qualified Data.Map.Strict as M
import qualified Data.Text                  as T
import           Text.Regex.TDFA

import           Drive.Crawl
import           Drive.Mail.IMAP
import           Drive.Mail.Confirmation

mailScraper :: EntityScraper (Text,Text,Text,Text)
mailScraper = EntityScraper
  { rootSelector = "body"
  , elementSelectors = M.fromList 
    [ ( "clientNb", text $ "table" @: [hasClass "deviceWidth"] //
                           "td" @: ["width" @= "220"]) 
    , ( "transactionNb", text $ "td" @: [hasClass "numberCde"] //
                                "table" // "td" // "p")
    , ( "barcode", attr "src" $ "table" @: [hasClass "deviceWidth"] //
                                "img" @: ["width" @= "250"]) 
    , ( "slot", text $ "td" @: [hasClass "numberCde"] //
                       "table" // "td" // "p" // "font")
  ]
  , entityMaker = makeMailInfo
  }

makeMailInfo :: Map Text Text -> Maybe (Text,Text,Text,Text)
makeMailInfo m = do
  clientNb <- M.lookup "clientNb" m
  tnb <- M.lookup "transactionNb" m
  tnb2 <- head $ getAllTextMatches (T.unpack tnb =~ re :: AllTextMatches [] String)
  barcode <- M.lookup "barcode" m
  slot <- M.lookup "slot" m

  return (clientNb, T.pack tnb2, barcode, slot)

  where
    re = "[0-9]+$" :: String

parse :: Mail -> Maybe MailInfo
parse Mail{..} = do
  (cNb, tNb, barcode, slot) <- join $ head $ entityScrap mailScraper (parseTags body)
  return $ MailInfo to cNb tNb barcode slot
