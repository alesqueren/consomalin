module Drive.Crawl.Auchan.Home (fetchCategoryUrls) where

import           Protolude       hiding (Selector)
import qualified Data.Text       as T
import           Drive.Crawl
import           Drive.Utils

data CategoryNotFoundException = CategoryNotFoundException deriving (Show, Typeable)
instance Exception CategoryNotFoundException


categoryDivSel :: Selector
categoryDivSel = "div" @: [hasClass "blocLayer", hasClass "float"] // "p"

categoryLink :: Selector -> Scraper Text Text
categoryLink _ = do
  relUrl <- attr "href" "a"
  return (T.pack $ T.unpack "https://www.auchandrive.fr" ++ T.unpack relUrl)

entryCategories :: Scraper Text [TextURI]
entryCategories = chroots categoryDivSel (categoryLink anySelector)

fetchCategoryUrls :: Crawl cr => Text -> cr [Text]
fetchCategoryUrls url =
  do
    goURI url
    _ <- getHtml []
    tags <- getPageTags []
    maybeOrThrow CategoryNotFoundException $ scrape entryCategories tags
