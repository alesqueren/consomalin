module Drive.Mongo (MongoResource(..), doSelectOne, doSelect, doInsert, doAggregate) where

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
  doc <- withMongoPipe (host $ T.unpack h) doAction
  maybeOrThrow DocNotFoundException $ (cast' . val) doc

  where
    (dbName, colName) = getPath r
    action = findOne (select s colName)
    doAction pipe = access pipe master dbName action

doSelect :: (Queryable r, Val v) => r -> (Text -> Query) -> IO [v]
doSelect r mkQuery = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  docs <- withMongoPipe (host $ T.unpack h) doAction
  return $ mapMaybe (cast' . val) docs

  where
    (dbName, colName) = getPath r
    action = rest =<< find (mkQuery colName)
    doAction pipe = access pipe master dbName action

doInsert :: (Queryable r, Val v) => r -> [v] -> IO ()
doInsert r values = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  e <- withMongoPipe (host $ T.unpack h) doAction
  print e

    where
      (dbName, colName) = getPath r
      docs = map (typed . val) values
      action = insertMany_ colName docs
      doAction pipe = access pipe master dbName action

doAggregate :: (Queryable r, Val v) => r -> [Document] -> IO [v]
doAggregate r query = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  docs <- withMongoPipe (host $ T.unpack h) doAction
  return $ mapMaybe (cast' . val) docs

  where
    (dbName, colName) = getPath r 
    action = aggregate colName query
    doAction pipe = access pipe master dbName action
