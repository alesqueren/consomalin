{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Drive.Crawl
  ( module Drive.Crawl
  , module X
  ) where

import           Protolude               hiding (try)

import           Network.HTTP.Conduit    as X
import           Network.HTTP.Types.Header

import           Network.URI
import           Text.HTML.Scalpel.Core  as X

import           Control.Lens            (makeLenses, use, (.=))
import           Control.Monad.Catch
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy.Encoding as LT
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
  httpRequest :: TextURI -> ByteString -> RequestHeaders -> ByteString -> m (Response LByteString)

-- | Compute an absolute or relative uri like a browser would do
computeURI :: Maybe URI -> TextURI -> Maybe URI
computeURI Nothing tUri = parseURI $ T.unpack tUri
computeURI (Just baseUri) tUri = do
  nu <- parseURIReference $ T.unpack tUri
  return $ if uriIsAbsolute nu
    then nu
    else nu `relativeTo` baseUri

-- |Navigate from an URI (absolute or relative to the current page)
goURI :: Crawl cr => TextURI -> cr ()
goURI u = do
  mcu <- curUri
  nUri <- maybeOrThrow InvalidURIException (computeURI mcu u)
  chUri nUri

{-
getHtml :: Crawl cr => RequestHeaders -> cr Text
getHtml headers = do
  resp <- getHttp headers 
  return . toStrict . decodeUtf8 . responseBody $ resp

getPageTags :: Crawl cr => RequestHeaders -> cr [Tag Text]
getPageTags headers = do
  resp <- getHttp headers

  return . parseTags . toStrict . decodeUtf8 . responseBody $ resp
-}

httpReqText :: Crawl cr => TextURI -> ByteString -> RequestHeaders -> Text -> cr Text
httpReqText tUri met h body = do
  resp <- httpRequest tUri met h (T.encodeUtf8 body)
  return . toStrict . LT.decodeUtf8 . responseBody $ resp

postText :: Crawl cr => TextURI -> RequestHeaders -> Text -> cr Text
postText tUri = httpReqText tUri "POST"

getText :: Crawl cr => TextURI -> RequestHeaders -> cr Text
getText tUri h = httpReqText tUri "GET" h ""

-- |Get the tags of a page
getPage :: Crawl cr => TextURI -> cr [Tag Text]
getPage tUri = do
  resp <- getText tUri []
  return . parseTags $ resp

{-
   NetCrawl: crawl with http requests
-}

-- |Internal session state for a net crawler
data NetSession = NetSession
  { _manager :: !Manager
  , _cookies :: !CookieJar
  , _cUri    :: !(Maybe URI)
  , _defHeaders :: RequestHeaders
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
                               , _defHeaders = []
                               }

-- |NetCrawl: a crawler which executes real http requests
instance Crawl NetCrawl where
  curUri = use cUri
  chUri = (.=) cUri . Just
  httpRequest tUri met headers body = do
    man <- use manager
    cooks <- use cookies
    baseUri <- use cUri
    defH <- use defHeaders

    uri <- maybeOrThrow InvalidURIException $ computeURI baseUri tUri

    req <- parseRequest $ show uri

    -- DANGER: header fields can be duplicated, read RFC 2616 section 4.2
    let req' = req { cookieJar = Just cooks
                   , requestHeaders = defH <> headers
                   , method = met
                   , requestBody = RequestBodyBS body
                   }

    resp <- X.httpLbs req' man
    cookies .= responseCookieJar resp
    return resp

-- |Run a NetCrawl session and returns the result in IO
runNetCrawl :: Exception e => Manager -> NetCrawl a -> IO (Either e a)
runNetCrawl man (NetCrawl cr) = try $ evalStateT cr (newNetSession man)
