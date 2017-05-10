{-# LANGUAGE DeriveGeneric #-}

module Drive.Transaction (Transaction(..), TStatus(..), findTransaction, changeStatus) where

import           Protolude hiding (Product, product)
import           Database.MongoDB
import           Drive.Bs.Mongo

data TStatus = Transferring | Done
  deriving (Typeable, Show, Eq, Generic)

mongoShow :: TStatus -> Text
mongoShow Transferring = "transferring"
mongoShow Done = "done"
-- instance Show TStatus where
--   show Transferring = (show "transferring" :: Text)
--  show Done = "done"

instance Val TStatus where
  val _ = val ([] :: [Text])
  cast' (String "transferring") = Just Transferring
  cast' (String "done") = Just Done
  cast' _ = Nothing


data Transaction = Transaction
  { driveUsername :: !Text
  , drivePassword :: !Text
  , slot :: !Text
  , status :: TStatus
  , basket :: [(Text, Int64)]
  }
  deriving (Typeable, Show, Eq, Generic)

extractPd :: Document -> Maybe (Text, Int64)
extractPd doc = do
  pd <- lookup "product" doc
  pid <- lookup "id" pd
  qty <- lookup "quantity" doc
  return (pid, qty)
 
instance Val Transaction where
  val t = val [ "status" =: status t ] -- todo: finish

  cast' (Doc doc) = do
    du <- lookup "_id" doc :: Maybe Text
    p <- lookup "password" doc :: Maybe Text

    t <- lookup "transaction" doc
    s <- lookup "status" t :: Maybe TStatus

    sl <- lookup "slot" t
    slId <- lookup "id" sl :: Maybe Text

    wishGroups <- lookup "wishGroups" t
    let b = mapMaybe extractPd $ concat $ mapMaybe (lookup "wishes") wishGroups

    return $ Transaction du p slId s b

  cast' _ = Nothing

findTransaction :: Text -> Text -> IO (Maybe Transaction)
findTransaction uid tid = do
  ts <- doAggregate UserResource query
  return $ head ts
    where
      query = [ [ "$match" =: [ "_id" =: uid ] ]
              , [ "$unwind" =: ("$transactions" :: Text)]
              , [ "$match" =: [ "transactions.id" =: tid ] ]
              , [ "$project" =: [ "transaction" =: ("$transactions" :: Text)
                                , "password" =: ("$password" :: Text) ] ] ]

changeStatus :: Text -> Text -> TStatus -> IO ()
changeStatus uid tid st =
  doModify UserResource [(sel, doc, [])]
    where 
      sel = [ "_id" =: uid, "transactions.id" =: tid ]
      doc = [ "$set" =: [ "transactions.$.status" =: mongoShow st] ]
