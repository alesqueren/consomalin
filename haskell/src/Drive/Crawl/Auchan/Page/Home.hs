module Drive.Crawl.Auchan.Page.Home (load, extractCategoryUrls) where

import           Protolude       hiding (Selector)
import qualified Data.Text       as T
import           Text.HTML.TagSoup
import           Drive.Crawl

load :: Text -> Crawl [Tag Text]
load url = requestTag $ Req url "GET" [] ""

categoryDivSel :: Selector
categoryDivSel = "div" @: [hasClass "blocLayer", hasClass "float"] // "p"

categoryLink :: Selector -> Scraper Text Text
categoryLink _ = do
  relUrl <- attr "href" "a"
  return (T.pack $ T.unpack "https://www.auchandrive.fr" ++ T.unpack relUrl)

entryCategories :: Scraper Text [TextURI]
entryCategories = chroots categoryDivSel (categoryLink anySelector)

-- TODO: use real url Type (in and out)
extractCategoryUrls :: [Tag Text] -> [Text]
extractCategoryUrls tags =
  fromMaybe [] $ scrape entryCategories tags
