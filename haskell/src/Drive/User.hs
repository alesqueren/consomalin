module Drive.User (Transaction(..), findTransaction) where

import           Protolude hiding (Product, product)
import           Drive.Mongo

import           Database.MongoDB


data Product = Product
  { pid :: !Text
  , quantity :: Int64
  }
  deriving (Typeable, Show, Eq)

instance Val Product where
  val p = val [ "id" =: pid p
              , "quantity" =: quantity p
              ]
  cast' (Doc doc) = do
    id <- lookup "id" doc :: Maybe Text
    q <- lookup "quantity" doc :: Maybe Int64
    return $ Product id q
  cast' _ = Nothing


data Wish = Wish
  { wName :: !Text
  , selected :: Bool
  , product :: Maybe Product
  }
  deriving (Typeable, Show, Eq)

instance Val Wish where
  val w = val [ "name" =: wName w
              , "select" =: selected w
              , "product" =: val (product w)
              ]
  cast' (Doc doc) = do
    n <- lookup "name" doc :: Maybe Text
    s <- lookup "selected" doc :: Maybe Bool
    p <- lookup "product" doc :: Maybe (Maybe Product)
    return $ Wish n s p
  cast' _ = Nothing


data WishGroups = WishGroups 
  { wgName :: !Text
  , wishes :: [Wish]
  }
  deriving (Typeable, Show, Eq)

instance Val WishGroups where
  val wg = val [ "name" =: wgName wg
               , "wishes" =: val (wishes wg)
               ]
  cast' (Doc doc) = do
    n <- lookup "name" doc :: Maybe Text
    ws <- lookup "wishes" doc :: Maybe [Value]
    return $ WishGroups n $ mapMaybe cast' ws
  cast' _ = Nothing

data User = User
  { uid :: !Text
  , password :: !Text
  , slotTime :: Maybe Text
  , wishGroups :: [WishGroups]
  }
  deriving (Typeable, Show, Eq)

instance Val User where
  val u = val [ "_id" =: uid u
              , "password" =: password u
              , "slotTime" =: slotTime u
              , "wishGroups" =: map val (wishGroups u)
              ]
  cast' (Doc doc) = do
    id <- lookup "_id" doc :: Maybe Text
    p <- lookup "password" doc :: Maybe Text
    wg <- lookup "wishGroups" doc :: Maybe [Value]
    return $ User id p (lookup "slotTime" doc) $ mapMaybe cast' wg

  cast' _ = Nothing

data Transaction = Transaction
  { username :: !Text
  , drivePassword :: !Text
  , driveSlotTime :: !Text
  , basket :: [(Text, Int64)]
  }
  deriving (Typeable, Show, Eq)


getTransaction :: User -> Maybe Transaction
getTransaction u = do
  st <- slotTime u
  Just $ Transaction (uid u) (password u) st b
  where
    b = concatMap getWishGroupPds (wishGroups u)
    getWishGroupPds wg = mapMaybe getPd (wishes wg)
    getPd w = case product w of
                Nothing -> Nothing
                Just p -> Just (pid p, quantity p)

findTransaction :: Text -> IO (Maybe Transaction)
findTransaction id = do
  u <- doSelectOne UserResource ["_id" =: id]
  return $ getTransaction u
