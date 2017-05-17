{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Login (login) where

import           Protolude       hiding (Selector)
import           Text.HTML.TagSoup
import           Control.Monad.Catch
import           Network.HTTP.Types.Status (statusCode)

import           Drive.Crawl
import           Drive.Crawl.Account

data LoginException = LoginException deriving (Show, Typeable)
instance Exception LoginException


load :: Crawl [Tag Text] 
load =
  requestTag $ Req "https://www.auchandrive.fr/drive/client/identification" "GET" [] ""

login :: Account -> Crawl ()
login acc = do
  $(logDebug) ("login" <> show acc)

  _ <- load
  res <- request $ Req url "POST" header httpData

  -- a successful identification must set new cookies
  when ((statusCode . responseStatus $ res) /= 200 ||
    all (\h -> fst h /= "Set-Cookie") (responseHeaders res)) $
    throwM LoginException

  return ()

    where
      url = "https://www.auchandrive.fr/drive/client/identification.formidentification"
      -- TODO: Get from html: #formIdentification > div:nth-child(1) 
      formdata = "t%3Aformdata=XviXyDb4kGDnwAyJ6fsR0LFe0p4%3D%3AH4sIAAAAAAAAAFvzloG1XJVBOTknMzWvRN8zBUhmpmUmJ5Zk5udZpeYmZuaUJeZkpiSWpBYXMZjmF6XrJRYkJmek6pUkFqQWlxRVmuol5xel5mQm6SUlFqfqOSYBBROTS9wyU3NSVIJTS0oLVEMPcz8UPf6HiYHRh4E7OT%2BvpCg%2Fxy8xN7WEQcgnK7EsUT8nMS9dP7ikKDMv3bqioISBF2xxGNRi4t3nSKr7Aoryk1OLi4NLk3Izi4uBRh5el2KS9m3eOSYGhoqCcg0GNewWFyQWF5fnF6XA7S5kqGNgKGEQgEnA3U60ESATWMvlGGSwKy8GObEE6EcHvH5Mzs8tyM8D6izWA3uqBNOLM4M%2FSW7d0uLMxMDkw8ABsc0zBWQ9KHpSc1JzgQKg6AELgaKDA2J5vCGCaQAALR8OljkCAAA%3D"
      httpData = formdata <> "&emailValidate=" <> driveUser acc <> "&passwordValidate=" <> drivePass acc <> "&t%3Asubmit=%5B%22submit_1%22%2C%22submit_0%22%5D&t%3Azoneid=identification"
      header = [("User-Agent", "curl/7.53.1" )
               , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
               , ("X-Requested-With", "XMLHttpRequest")
               ]
