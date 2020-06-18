{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Orders (getLastTid) where

import           Protolude       hiding (Selector)
import           Text.HTML.TagSoup
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Drive.Crawl

data LoadException = LoadException deriving (Show, Typeable)
instance Exception LoadException

lastTidScraper :: EntityScraper Text
lastTidScraper = EntityScraper
  { rootSelector = "body"
  , elementSelectors = M.fromList [ 
    ( "lastTid", text $ "div" @: ["id" @= "central-container"] //
                        "div" @: [hasClass "derniere-commande"] //
                        "p"   @: [hasClass "n-commande"])
    ]
  , entityMaker = M.lookup "lastTid"
  -- , entityMaker = show
  }

load :: Crawl [Tag Text]
load = requestTag $ Req url "GET" [] ""
  where url = "https://www.auchandrive.fr/drive/client/mescommandes"

getLastTid :: Crawl Text 
getLastTid = do
  $(logDebug) "get last transaction id"
  page <- load
  tid <- maybeOrThrow LoadException $ join $ head $ entityScrap lastTidScraper page
  return $ T.replace "COMMANDE N°" "" tid
