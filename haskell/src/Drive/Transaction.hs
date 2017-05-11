{-# LANGUAGE DeriveGeneric #-}

module Drive.Transaction (Transaction(..)
                         , module Drive.Transaction.Status
                         , mongoFind
                         ) where

import           Protolude hiding (Product, product)
import           Database.MongoDB
import           Drive.Transaction.Status
import           Drive.Bs.Mongo

data Transaction = Transaction
  { driveUsername :: !Text
  , drivePassword :: !Text
  , slot :: !Text
  , status :: Status
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
  -- TODO: complete function
  val t = val [ "status" =: status t ]

  cast' (Doc doc) = do
    du <- lookup "_id" doc :: Maybe Text
    p <- lookup "password" doc :: Maybe Text

    t <- lookup "transaction" doc
    s <- lookup "status" t :: Maybe Status

    sl <- lookup "slot" t
    slId <- lookup "id" sl :: Maybe Text

    wishGroups <- lookup "wishGroups" t
    let b = mapMaybe extractPd $ concat $ mapMaybe (lookup "wishes") wishGroups

    return $ Transaction du p slId s b

  cast' _ = Nothing

mongoFind :: Text -> Text -> IO (Maybe Transaction)
mongoFind uid tid = do
  ts <- doAggregate UserResource query
  return $ head ts
    where
      query = [ [ "$match" =: [ "_id" =: uid ] ]
              , [ "$unwind" =: ("$transactions" :: Text)]
              , [ "$match" =: [ "transactions.id" =: tid ] ]
              , [ "$project" =: [ "transaction" =: ("$transactions" :: Text)
                                , "password" =: ("$password" :: Text) ] ] ]
