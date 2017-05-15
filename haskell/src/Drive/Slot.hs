{-# LANGUAGE DeriveGeneric #-}

module Drive.Slot (Slot(..), SlotStatus(..)) where

import           Protolude
import           Data.Aeson
import           Data.Time


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
