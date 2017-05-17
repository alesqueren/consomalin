module Drive.Crawl.Auchan.Page.Landing (load, doChooseDrive) where

import           Protolude       hiding (Selector)
import           Prelude                    (String)
import qualified Data.Text       as T
import           Text.HTML.TagSoup
import           Drive.Crawl
import           Text.Regex.TDFA

type ShopName = Text

data ShopNotFoundException = ShopNotFoundException deriving (Show, Typeable)
instance Exception ShopNotFoundException

data SelectorType = ShopS | ShopLinkS
getSel :: SelectorType -> Selector
getSel ShopS     = "div" @: ["id" @= "liste_drives"] // "li"
getSel ShopLinkS = "a" @: [hasClass "entreDrive", hasClass "full"]

load :: Crawl [Tag Text] 
load = requestTag $ Req "http://www.auchandrive.fr" "GET" [] ""

findShopUrl :: Text -> [Text] -> Maybe Text
findShopUrl sn urls = 
  do
    res <- join $ head $ filter isJust $ map (head . getAllTextSubmatches . matches) urls
    return $ T.pack $ "http://www.auchandrive.fr" <> res
    where 
      re = "^/drive.*/" ++ T.unpack sn ++ "/" :: String
      matches x = T.unpack x =~ re :: AllTextSubmatches [] String

entryShopLink :: Text -> Scraper Text (Maybe Text)
entryShopLink sn = do
  urls <- chroots (getSel ShopS) (attr "href" $ getSel ShopLinkS)
  return $ findShopUrl sn urls

-- |Directly goes into a drive from a shop name
-- after that, the same Crawl object can be used to crawl in parallel
doChooseDrive :: ShopName -> Crawl Text
doChooseDrive shop = do
  tags <- load

  shopUrl <- maybeOrThrow ShopNotFoundException $ join $ scrape (entryShopLink shop) tags
 
  -- set auchanCook cookie
  _ <- request $ Req "http://www.auchandrive.fr/drive/faq" "GET" [] ""

  -- validate selection
  _ <- request $ Req shopUrl "GET" [] ""

  -- set new jsession cookie
  _ <- request $ Req shopUrl "GET" [] ""

  return shopUrl
