{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Drive.Crawl
  ( module Drive.Crawl
  , module X
  ) where

import           Protolude               hiding (decodeUtf8, try)

import           Network.HTTP.Conduit    as X
import           Network.HTTP.Types.Header

import           Network.URI
import           Text.HTML.Scalpel.Core  as X

import           Control.Lens            (makeLenses, use, (.=))
import           Control.Monad.Catch
import qualified Data.Text               as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Drive.Utils
import           Drive.Types             as X
import           Text.HTML.TagSoup


-- |Some exception types for this module
-- Should these provide an extra string description?
data CrawlException = NoURIException | InvalidURIException
  deriving (Show, Typeable)

instance Exception CrawlException

-- |The Crawl typeclass which provides a generic interface
-- for a crawler
class (Monad m, MonadIO m, MonadThrow m) => Crawl m where
  curUri :: m (Maybe URI)
  chUri :: URI -> m ()
  getHttp :: RequestHeaders -> m (Response LByteString)

-- |Navigate from an URI (absolute or relative to the current page)
goURI :: Crawl cr => TextURI -> cr ()
goURI u = do
  mcu <- curUri
  case mcu of
    Nothing -> do
      nu <- maybeOrThrow InvalidURIException (parseURI u')
      chUri nu
    Just cu -> do
      nu <- maybeOrThrow InvalidURIException (parseURIReference u')
      if uriIsAbsolute nu
         then chUri nu
         else chUri $ nu `relativeTo` cu
  where
    u' = T.unpack u

getHtml :: Crawl cr => RequestHeaders -> cr Text
getHtml headers = do
  resp <- getHttp headers 
  return . toStrict . decodeUtf8 . responseBody $ resp

-- |Get the tags of the current page
getPageTags :: Crawl cr => RequestHeaders -> cr [Tag Text]
getPageTags headers = do
  resp <- getHttp headers

  return . parseTags . toStrict . decodeUtf8 . responseBody $ resp


{-
   NetCrawl: crawl with http requests
-}

-- |Internal session state for a net crawler
data NetSession = NetSession
  { _manager :: !Manager
  , _cookies :: !CookieJar
  , _cUri    :: !(Maybe URI)
  }

makeLenses ''NetSession

newtype NetCrawl a = NetCrawl (StateT NetSession IO a)
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadState NetSession, MonadThrow)

-- |Create a new NetSession to use with a NetCrawl
newNetSession :: Manager -> NetSession
newNetSession man = NetSession { _manager = man
                               , _cookies = createCookieJar []
                               , _cUri = Nothing
                               }

-- |NetCrawl: a crawler which executes real http requests
instance Crawl NetCrawl where
  curUri = use cUri
  chUri = (.=) cUri . Just
  getHttp headers = do
    uri <- use cUri >>= maybeOrThrow NoURIException
    cooks <- use cookies
    man <- use manager

    req <- parseRequest $ show uri
    let req' = if null headers
                then req { cookieJar = Just cooks }
                else req { 
                  cookieJar = Just cooks,
                  requestHeaders = headers,
                  method = "POST" -- should be set elsewhere
                }

    resp <- X.httpLbs req' man
    cookies .= responseCookieJar resp
    return resp

-- |Run a NetCrawl session and returns the result in IO
runNetCrawl :: Exception e => Manager -> NetCrawl a -> IO (Either e a)
runNetCrawl man (NetCrawl cr) = try $ evalStateT cr (newNetSession man)
