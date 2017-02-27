module Drive.Mongo (MongoResource(..), doSelectOne) where

import           Protolude                    hiding (Product, (<>), find, sort)
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

doSelectOne :: (Queryable r, Val v) => r -> [Field] -> IO v
doSelectOne r s = do
  h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1"
  doc <- withMongoPipe (host $ T.unpack h) doAction
  maybeOrThrow DocNotFoundException $ (cast' . val) doc

  where
    (dbName, colName) = getPath r
    action = findOne (select s colName)
    doAction pipe = access pipe master dbName action

-- doSelect :: (Queryable r, Val v) => r -> [Field] -> IO [v]
-- doSelect r s = do
--   h <- fromEnvOr "MONGO_HOST" A.takeText "127.0.0.1" 
--   docs <- withMongoPipe (host $ T.unpack h) doAction
--   return $ mapMaybe (cast' . val) docs
-- 
--   where 
--     (dbName, colName) = getPath r
--     action = rest =<< find (select s colName)
--     doAction pipe = access pipe master dbName action

