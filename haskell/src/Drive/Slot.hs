{-# LANGUAGE DeriveGeneric #-}

module Drive.Slot (Slot(..), SlotStatus(..), ChoosenSlot(..), mongoFindChoosenSlot) where

import           Protolude
import           Data.Aeson
import           Data.Time
import           Database.MongoDB
import           Drive.Bs.Mongo

data Slot = Slot 
  { id :: Maybe Text
  , day :: Day
  , time :: TimeOfDay
  , status :: SlotStatus
  , attendanceLevel :: Maybe Float
  }
  deriving (Show, Generic)
instance ToJSON Slot

data SlotStatus = Past | Available | Busy
  deriving (Show, Generic)
instance ToJSON SlotStatus


newtype ChoosenSlot = ChoosenSlot Text
  deriving (Typeable, Show, Eq, Generic)

instance Val ChoosenSlot where
  -- TODO: complete function
  -- val b = val [ "totalPrice" =: totalPrice b ]

  -- | takes a UserResource Document
  cast' (Doc doc) = do
    a <- lookup "currentBasket" doc
    b <- lookup "slot" a
    c <- lookup "id" b
    return $ ChoosenSlot c

mongoFindChoosenSlot :: Text -> IO (Maybe ChoosenSlot)
mongoFindChoosenSlot uid =
  doSelectOne UserResource ["_id" =: uid]
