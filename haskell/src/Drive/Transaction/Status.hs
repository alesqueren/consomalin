{-# LANGUAGE DeriveGeneric #-}

module Drive.Transaction.Status (Status(..), mongoSet) where

import           Protolude hiding (Product, product)
import           Database.MongoDB
import           Drive.Bs.Mongo

data Status = Transferring | Done
  deriving (Typeable, Show, Eq, Generic)

-- TODO: same as val ?
mongoShow :: Status -> Text
mongoShow Transferring = "transferring"
mongoShow Done = "done"

instance Val Status where
  val _ = val ([] :: [Text])
  cast' (String "transferring") = Just Transferring
  cast' (String "done") = Just Done
  cast' _ = Nothing

mongoSet :: Text -> Text -> Status -> IO ()
mongoSet uid tid st =
  doModify UserResource [(sel, doc, [])]
    where 
      sel = [ "_id" =: uid, "transactions.id" =: tid ]
      doc = [ "$set" =: [ "transactions.$.status" =: mongoShow st] ]
