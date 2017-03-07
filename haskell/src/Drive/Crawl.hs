{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
--{-# LANGUAGE OverlappingInstances       #-}

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
import           Control.Monad.Logger    as X
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy.Encoding as LT
import           Drive.Utils
import           Drive.Types             as X
import           Text.HTML.TagSoup
import           System.Log.FastLogger

import           Control.Monad.Trans.Free.Church

import Data.Time.Clock
import Data.Time.Calendar

-- |Some exception types for this module
-- Should these provide an extra string description?
data CrawlException = NoURIException | InvalidURIException
  deriving (Show, Typeable)

instance Exception CrawlException

type Method = ByteString

data CrawlF next =
    CurUri (Maybe URI -> next)
  | ChUri URI next
  | HttpRequest TextURI Method RequestHeaders ByteString (Response LByteString -> next)
  | LogMsg Loc LogSource LogLevel LogStr next
  | Throw SomeException
  deriving (Functor)

type Crawl = F CrawlF

curUri :: MonadFree CrawlF m => m (Maybe URI)
curUri = liftF $ CurUri identity

chUri :: MonadFree CrawlF m => URI -> m ()
chUri u = liftF $ ChUri u ()

httpRequest :: MonadFree CrawlF m => TextURI -> Method -> RequestHeaders -> ByteString -> m (Response LByteString)
httpRequest tUri met headers body = liftF $ HttpRequest tUri met headers body identity

-- log :: MonadFree CrawlF m => Text -> m ()
-- log t = liftF $ Log t ()

instance {-# OVERLAPPING #-} MonadThrow Crawl where
  throwM e = liftF $ Throw (toException e)

instance MonadLogger Crawl where
  monadLoggerLog loc source level s = liftF $ LogMsg loc source level (toLogStr s) ()

-- | Compute an absolute or relative uri like a browser would do
computeURI :: Maybe URI -> TextURI -> Maybe URI
computeURI Nothing tUri = parseURI $ T.unpack tUri
computeURI (Just baseUri) tUri = do
  nu <- parseURIReference $ T.unpack tUri
  return $ if uriIsAbsolute nu
    then nu
    else nu `relativeTo` baseUri

-- |Navigate from an URI (absolute or relative to the current page)
goURI :: TextURI -> Crawl ()
goURI u = do
  mcu <- curUri
  nUri <- maybe (throwM InvalidURIException) return (computeURI mcu u)
  chUri nUri

httpReqText :: TextURI -> Method -> RequestHeaders -> Text -> Crawl Text
httpReqText tUri met h body = do
  resp <- httpRequest tUri met h (T.encodeUtf8 body)
  $(logDebug) (show $ responseStatus resp :: Text)
  return . toStrict . LT.decodeUtf8 . responseBody $ resp

postText :: TextURI -> RequestHeaders -> Text -> Crawl Text
postText tUri = httpReqText tUri "POST"

getText :: TextURI -> RequestHeaders -> Crawl Text
getText tUri h = httpReqText tUri "GET" h ""

-- |Get the tags of a page
getPage :: TextURI -> Crawl [Tag Text]
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

-- |Create a new NetSession to use with a NetCrawl
newNetSession :: Manager -> NetSession
newNetSession man = NetSession { _manager = man
                               , _cookies = createCookieJar []
                               , _cUri = Nothing
                               , _defHeaders = []
                               }

runNetCrawl :: (MonadThrow m, MonadIO m) => Manager -> FT CrawlF m a -> m a
runNetCrawl m c = evalStateT (iterTM go c) (newNetSession m)
  where
    -- go :: CrawlF (StateT NetSession m a) -> StateT NetSession m a
    go (CurUri f) = use cUri >>= f
    go (ChUri u f) = cUri .= Just u >> f
    go (HttpRequest tUri met headers body f) = f =<< do
      man <- use manager
      cooks <- use cookies
      -- putStrLn (show $ filter (\x -> cookie_name x == "JSESSIONID") cooks :: Text)
      --putStrLn (show cooks :: Text)

      baseUri <- use cUri
      defH <- use defHeaders

      uri <- maybeOrThrow InvalidURIException $ computeURI baseUri tUri

      req <- parseRequest $ show uri

      let past = UTCTime (ModifiedJulianDay 57818) (secondsToDiffTime 34000)
      let future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)
      
      let c1 = Cookie { cookie_name = "JSESSIONID"
                      , cookie_value = "9C916B17F67B9F1994FA44A6DA8E0965.drive03s"
                      , cookie_expiry_time = future
                      , cookie_domain = "www.auchandrive.fr"
                      , cookie_path = "/drive"
                      , cookie_creation_time = past
                      , cookie_last_access_time = past
                      , cookie_persistent = True
                      , cookie_host_only = True
                      , cookie_secure_only = False
                      , cookie_http_only = True
                      }

      let c2 = Cookie { cookie_name = "auchanCook"
                      , cookie_value = "\"954|\""
                      , cookie_expiry_time = future
                      , cookie_domain = "www.auchandrive.fr"
                      , cookie_path = "/"
                      , cookie_creation_time = past
                      , cookie_last_access_time = past
                      , cookie_persistent = False
                      , cookie_host_only = True
                      , cookie_secure_only = False
                      , cookie_http_only = True
                      }


      -- DANGER: header fields can be duplicated, read RFC 2616 section 4.2
      let req' = req { cookieJar = Just cooks
      -- let req' = req { cookieJar = Just $ createCookieJar [c1, c2]
                     , requestHeaders = defH <> headers
                     , method = met
                     , requestBody = RequestBodyBS body
                     }

      resp <- X.httpLbs req' man

      cookies .= responseCookieJar resp

      cooks2 <- use cookies
      putStrLn (show cooks2 :: Text)

      return resp
    go (LogMsg loc source level s f) = do

      putStr (fromLogStr $ defaultLogStr loc source level s)
      f
    go (Throw e) = throwIO e
