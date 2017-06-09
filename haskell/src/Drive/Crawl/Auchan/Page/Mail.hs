module Drive.Crawl.Auchan.Page.Mail (parse) where

import           Protolude
import           Prelude                    (String)
import           Text.HTML.TagSoup
import qualified Data.Map.Strict as M
import qualified Data.Text                  as T
import           Text.Regex.TDFA

import           Drive.Crawl
import           Drive.Mail.Smtp

mailScraper :: EntityScraper MailInfo
mailScraper = EntityScraper
  { rootSelector = "body"
  , elementSelectors = M.fromList 
    [ ( "clientNb", text $ "table" @: [hasClass "deviceWidth"] //
                           "td" @: ["width" @= "220"]) 
    , ( "transactionNb", text $ "td" @: [hasClass "numberCde"] //
                                "table" // "td" // "p")
    , ( "barcode", attr "src" $ "table" @: [hasClass "deviceWidth"] //
                                "img" @: ["width" @= "250"]) 
  ]
  , entityMaker = makeMailInfo
  }

makeMailInfo :: Map Text Text -> Maybe MailInfo
makeMailInfo m = do
  clientNb <- M.lookup "clientNb" m
  tnb <- M.lookup "transactionNb" m
  tnb2 <- head $ getAllTextMatches (T.unpack tnb =~ re :: AllTextMatches [] String)
  barcode <- M.lookup "barcode" m

  return $ MailInfo clientNb (T.pack tnb2) barcode

  where
    re = "[0-9]+$" :: String

parse :: Text -> Maybe MailInfo
parse mail =
  join $ head $ entityScrap mailScraper (parseTags mail)
