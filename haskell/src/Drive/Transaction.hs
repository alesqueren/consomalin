{-# LANGUAGE DeriveGeneric #-}

module Drive.Transaction (Transaction(..), findTransaction) where

import           Protolude hiding (Product, product)
import           Database.MongoDB
import           Drive.Mongo

data Transaction = Transaction
  { driveUsername :: !Text
  , drivePassword :: !Text
  , slot :: !Text
  , status :: !Text 
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
    s <- lookup "status" t :: Maybe Text

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
              , [ "$project" =: [ "transaction" =: "$transactions." <> tid 
                                , "password" =: ("$password" :: Text) ] ] ]
