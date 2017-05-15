module Drive.Crawl.Auchan.Page.Landing (load, doChooseDrive) where

import           Protolude       hiding (Selector)
import qualified Data.Text       as T
import           Text.HTML.TagSoup
import           Utils.Re
import           Drive.Crawl

type ShopName = Text

data ShopNotFoundException = ShopNotFoundException deriving (Show, Typeable)
instance Exception ShopNotFoundException

load :: Crawl [Tag Text] 
load = requestTag $ Req "http://www.auchandrive.fr" "GET" [] ""

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

-- |Directly goes into a drive from a shop name
-- after that, the same Crawl object can be used to crawl in parallel
doChooseDrive :: ShopName -> Crawl Text
doChooseDrive shop = do
  tags <- load

  shopRelUri <- maybeOrThrow ShopNotFoundException $ scrape (entryShopLink shop) tags
  let shopAbsUri = "http://www.auchandrive.fr" <> shopRelUri
 
  -- set auchanCook cookie
  _ <- request $ Req "http://www.auchandrive.fr/drive/faq" "GET" [] ""

  -- validate selection
  _ <- request $ Req shopAbsUri "GET" [] ""

  -- set new jsession cookie
  _ <- request $ Req shopAbsUri "GET" [] ""

  return shopAbsUri
