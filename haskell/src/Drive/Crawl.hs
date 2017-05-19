{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Drive.Crawl
  ( module Drive.Crawl
  , module X
  ) where

import           Protolude               hiding (try, Selector)

import           Network.HTTP.Conduit    as X
import           Network.HTTP.Types.Header as X

import           Network.URI

import           Text.HTML.Scalpel.Core  as X
import           Control.Lens            (makeLenses, use, (.=))
import           Control.Monad.Catch
import           Control.Monad.Logger    as X
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Aeson (FromJSON, decode')
import           Text.HTML.TagSoup
import           System.Log.FastLogger
import           Control.Monad.Trans.Free.Church
import           Conduit

import           Utils.Misc              as X
import           Drive.Types             as X

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

-- curUri :: MonadFree CrawlF m => m (Maybe URI)
-- curUri = liftF $ CurUri identity

-- chUri :: MonadFree CrawlF m => URI -> m ()
-- chUri u = liftF $ ChUri u ()

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

-- TODO: use real url type
-- |Navigate from an URI (absolute or relative to the current page)
-- goURI :: TextURI -> Crawl ()
-- goURI u = do
--   mcu <- curUri
--   nUri <- maybe (throwM InvalidURIException) return (computeURI mcu u)
--   chUri nUri

data Req = Req 
  { uri :: TextURI
  , met :: Method
  , headers :: RequestHeaders 
  , body :: Text
  }

request :: Req -> Crawl (Response LByteString)
request req = do
  liftF $ HttpRequest (uri req) (met req) (headers req) (T.encodeUtf8 $ body req) identity

requestTag :: Req -> Crawl [Tag Text]
requestTag req = do
  resp <- request req
  return . parseTags . toStrict . LT.decodeUtf8 . responseBody $ resp

requestJson :: Req -> Crawl BL.ByteString
requestJson req = do
  resp <- request req
  return . BL.fromStrict . encodeUtf8 . toStrict . LT.decodeUtf8 . responseBody $ resp

-- -- TODO: rm
-- httpRequest :: MonadFree CrawlF m => TextURI -> Method -> RequestHeaders -> ByteString -> m (Response LByteString)
-- httpRequest tUri met headers body = liftF $ HttpRequest tUri met headers body identity
-- 
-- -- TODO: rm
-- httpReqText :: TextURI -> Method -> RequestHeaders -> Text -> Crawl Text
-- httpReqText tUri met h body = do
--   resp <- httpRequest tUri met h (T.encodeUtf8 body)
--   return . toStrict . LT.decodeUtf8 . responseBody $ resp
-- 
-- -- TODO: rm
-- postText :: TextURI -> RequestHeaders -> Text -> Crawl Text
-- postText tUri = httpReqText tUri "POST"
-- 
-- -- TODO: rm
-- getText :: TextURI -> RequestHeaders -> Crawl Text
-- getText tUri h = httpReqText tUri "GET" h ""
-- 
-- -- TODO: rm
-- -- |Get the tags of a page
-- getPage :: TextURI -> Crawl [Tag Text]
-- getPage tUri = do
--   resp <- getText tUri []
--   return . parseTags $ resp


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

      -- cooks2 <- use cookies
      -- putStrLn (show cooks2 :: Text)

      return resp
    go (LogMsg loc source level s f) = do

      putStr (fromLogStr $ defaultLogStr loc source level s)
      f
    go (Throw e) = throwIO e


runConduitCrawl :: ConduitM () Void (FT CrawlF IO) a -> IO a
runConduitCrawl pipe = do
  man <- newManager tlsManagerSettings
  runNetCrawl man $ runConduit pipe


-- |apply an extractor function on a json page
extractFromJson :: (FromJSON page) => (page -> [a]) -> Crawl BL.ByteString -> Crawl [a]
extractFromJson extract page = do
  p <- page
  return $ maybe [] extract $ (decode' p)

data EntityScraper a = EntityScraper
  { rootSelector     :: Selector
  , elementSelectors :: Map Text (Scraper Text Text)
  , entityMaker      :: Map Text Text -> Maybe a
  }

toto :: Map Text (Scraper Text Text) -> (Map Text Text -> Maybe a) -> Scraper Text (Maybe a)
toto selectors maker =
  do
    elementTxts <- mapM identity selectors
    return $ maker elementTxts

tata :: EntityScraper a -> Scraper Text [Maybe a]
tata (EntityScraper rs es em) = 
  chroots rs (toto es em) 

entityScrape :: EntityScraper a -> [Tag Text] -> [Maybe a]
entityScrape scraper page =
  fromMaybe [] $ scrape (tata scraper) page
