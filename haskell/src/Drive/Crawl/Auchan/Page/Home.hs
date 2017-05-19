module Drive.Crawl.Auchan.Page.Home (load, extractCategoryUrls) where

import           Protolude       hiding (Selector)
import qualified Data.Map.Strict as M
import           Text.HTML.TagSoup
import           Drive.Crawl

categoryUrlScraper :: EntityScraper Text
categoryUrlScraper = EntityScraper 
  { rootSelector = "div" @: [hasClass "blocLayer", hasClass "float"] // "p"
  , elementSelectors = M.fromList [("urls", attr "href" "a")]
  , entityMaker = M.lookup "urls"
  }

load :: Text -> Crawl [Tag Text]
load url = requestTag $ Req url "GET" [] ""

-- TODO: use real url Type (in and out)
extractCategoryUrls :: Text -> Crawl [Text]
extractCategoryUrls url = do
  page <- load url
  return $ catMaybes $ entityScrap categoryUrlScraper page
