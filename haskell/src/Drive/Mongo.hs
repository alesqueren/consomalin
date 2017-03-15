module Drive.Mongo (MongoResource(..), doSelectOne, doSelect, doInsert, doModify, doAggregate) where

import           Protolude                    hiding (Product, (<>), find, sort, Selector)
import           Database.MongoDB
import           Drive.Utils
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T


data MongoException = DocNotFoundException
  deriving (Show, Typeable)
instance Exception MongoException

data MongoResource = UserResource | ProductResource

class Queryable a where
  getPath :: a -> (Text, Text)

instance Queryable MongoResource where
  getPath UserResource = ("users", "user")
  getPath ProductResource = ("auchan", "product")

withMongoPipe :: Host -> (Pipe -> IO a) -> IO a
withMongoPipe h = bracket (connect h) close

-- TODO: get host only once
doSelectOne :: (Queryable r, Val v) => r -> [Field] -> IO v
doSelectOne r s = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  doc <- withMongoPipe (host $ T.unpack h) doWithPipe
  maybeOrThrow DocNotFoundException $ (cast' . val) doc

  where
    (dbName, colName) = getPath r
    action = findOne (select s colName)
    doWithPipe pipe = access pipe master dbName action

doAction :: Val v => (Pipe -> IO [Document]) -> IO [v]
doAction a = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  docs <- withMongoPipe (host $ T.unpack h) a
  return $ mapMaybe (cast' . val) docs

doSelect :: (Queryable r, Val v) => r -> (Text -> Query) -> IO [v]
doSelect r mkQuery = doAction doWithPipe
  where
    (dbName, colName) = getPath r
    action = rest =<< find (mkQuery colName)
    doWithPipe pipe = access pipe master dbName action

doInsert :: (Queryable r, Val v) => r -> [v] -> IO ()
doInsert r values = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  e <- withMongoPipe (host $ T.unpack h) doWithPipe
  print e

    where
      (dbName, colName) = getPath r
      docs = map (typed . val) values
      action = insertMany_ colName docs
      doWithPipe pipe = access pipe master dbName action

doModify :: (Queryable r) => r -> [(Selector, Document, [UpdateOption])] -> IO ()
doModify r query = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  _ <- withMongoPipe (host $ T.unpack h) doWithPipe
  return ()

  where
    (dbName, colName) = getPath r 
    action = updateMany colName query
    doWithPipe pipe = access pipe master dbName action

doAggregate :: (Queryable r, Val v) => r -> [Document] -> IO [v]
doAggregate r query = doAction doWithPipe
  where
    (dbName, colName) = getPath r 
    action = aggregate colName query
    doWithPipe pipe = access pipe master dbName action
