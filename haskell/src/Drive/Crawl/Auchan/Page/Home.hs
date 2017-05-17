module Drive.Crawl.Auchan.Page.Home (load, extractCategoryUrls) where

import           Protolude       hiding (Selector)
import qualified Data.Text       as T
import           Text.HTML.TagSoup
import           Drive.Crawl

data SelectorType = CategoryS | CategoryUrlS 
getSel :: SelectorType -> Selector
getSel CategoryS    = "div" @: [hasClass "blocLayer", hasClass "float"] // "p"
getSel CategoryUrlS = "a"


load :: Text -> Crawl [Tag Text]
load url = requestTag $ Req url "GET" [] ""

categoryLink :: Selector -> Scraper Text Text
categoryLink _ = do
  relUrl <- attr "href" $ getSel CategoryUrlS
  return (T.pack $ T.unpack "https://www.auchandrive.fr" ++ T.unpack relUrl)

entryCategories :: Scraper Text [TextURI]
entryCategories = chroots (getSel CategoryS) (categoryLink anySelector)

-- TODO: use real url Type (in and out)
extractCategoryUrls :: [Tag Text] -> [Text]
extractCategoryUrls tags =
  fromMaybe [] $ scrape entryCategories tags
