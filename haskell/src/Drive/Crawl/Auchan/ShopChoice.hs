module Drive.Crawl.Auchan.ShopChoice (chooseDrive) where

import           Protolude       hiding (Selector)
import qualified Data.Text       as T
import           Drive.Crawl
import           Drive.Re
import           Drive.Utils

type ShopName = Text

data ShopNotFoundException = ShopNotFoundException deriving (Show, Typeable)
instance Exception ShopNotFoundException

shopListSel :: Selector
shopListSel = "div" @: ["id" @= "liste_drives"] // "ul"

shopLinkSel :: Selector
shopLinkSel = shopListSel // "a" @: [hasClass "entreDrive", hasClass "full"]

entryShopLink :: Text -> Scraper Text Text
entryShopLink sn = do
  cre <- compileReM shopLinkRe
  chroot shopLinkSel (attr "href" $ matchHref cre)
  where
    matchHref :: Regex -> Selector
    matchHref r = "a" @: ["href" @=~ r ]
    shopLinkRe =  "^/drive.*/" ++ T.unpack sn ++ "/"


-- |Returns a link to a shop from its name
selectCityShop :: Crawl cr => ShopName -> cr TextURI
selectCityShop shop = do
  goURI "https://www.auchandrive.fr"
  startP <- getPageTags []

  -- get the shop link
  maybeOrThrow ShopNotFoundException $ scrape (entryShopLink shop) startP

-- |Directly goes into a drive from a shop name
-- after that, the same Crawl object can be used to crawl in parallel
chooseDrive :: Crawl cr => ShopName -> cr Text
chooseDrive shop = do
  shopURI <- selectCityShop shop
 
  -- does not work if commented. why ?
  goURI "http://www.auchandrive.fr/drive/faq"
  _ <- getHtml []

  return $ T.append "http://www.auchandrive.fr" shopURI
