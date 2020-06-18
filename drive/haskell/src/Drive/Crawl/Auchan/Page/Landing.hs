module Drive.Crawl.Auchan.Page.Landing (load, doChooseDrive) where

import           Protolude       hiding (Selector)
import           Prelude                    (String)
import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import           Text.Regex.TDFA
import           Text.HTML.TagSoup

import           Drive.Crawl

type ShopName = Text

data ShopNotFoundException = ShopNotFoundException deriving (Show, Typeable)
instance Exception ShopNotFoundException

shopUrlScraper :: EntityScraper Text
shopUrlScraper = EntityScraper
  { rootSelector = "div" @: ["id" @= "liste_drives"] // "li"
  , elementSelectors = M.fromList 
    [ ("url", attr "href" $ "a" @: [hasClass "entreDrive", hasClass "full"]) ]
  , entityMaker = M.lookup "url"
  }

load :: Crawl [Tag Text] 
load = requestTag $ Req "http://www.auchandrive.fr" "GET" [] ""

findShopUrl :: ShopName -> [Text] -> Maybe Text
findShopUrl shopName urls = 
  do
    res <- join $ head $ filter isJust $ map (head . getAllTextSubmatches . matches) urls
    return $ T.pack $ "http://www.auchandrive.fr" <> res
    where 
      re = "^/drive.*/" ++ T.unpack shopName ++ "/" :: String
      matches x = T.unpack x =~ re :: AllTextSubmatches [] String

-- |Directly goes into a drive from a shop name
-- after that, the same Crawl object can be used to crawl in parallel
doChooseDrive :: ShopName -> Crawl Text
doChooseDrive shopName = do
  page <- load

  shopUrl <- 
    maybeOrThrow ShopNotFoundException $
      findShopUrl shopName $
        catMaybes $ 
          entityScrap shopUrlScraper page

  -- set auchanCook cookie
  _ <- request $ Req "http://www.auchandrive.fr/drive/faq" "GET" [] ""

  -- validate selection
  _ <- request $ Req shopUrl "GET" [] ""

  -- set new jsession cookie
  _ <- request $ Req shopUrl "GET" [] ""

  return shopUrl
