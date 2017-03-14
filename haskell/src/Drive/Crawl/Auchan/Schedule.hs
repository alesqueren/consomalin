{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Schedule (getSchedule, selectSchedule, Slot, SlotInfo, makeSlot) where

import           Protolude       hiding (Selector, inits)
import           Prelude                    (String)
import           Drive.Crawl
import           Drive.Utils
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text       as T
import           Data.Aeson
import           Text.Regex.TDFA
import           Text.HTML.TagSoup

import Data.Time

data SlotStatus = Past | Available | Busy
  deriving (Show, Generic)
instance ToJSON SlotStatus

data Slot = Slot 
  { id :: Maybe Text
  , day :: Day
  , time :: TimeOfDay
  , status :: SlotStatus
  , attendanceLevel :: Float
  }
  deriving (Show, Generic)
instance ToJSON Slot

data SlotInfo = SlotInfo 
  { sid :: Maybe Text
  , sDayFromNow :: Integer
  , sTime :: TimeOfDay
  , sStatus :: SlotStatus
  }
  deriving (Show, Generic)
instance ToJSON SlotInfo

type ParsedSlotInfo = (Maybe Text, TimeOfDay, SlotStatus)

makeSlotInfo :: Integer -> ParsedSlotInfo -> SlotInfo
makeSlotInfo day (id, time, status) =
  SlotInfo id day time status

makeSlot :: Day -> SlotInfo -> Slot
makeSlot currDay si =
  Slot (sid si) d (sTime si) (sStatus si) 0
    where 
      d = addDays (sDayFromNow si) currDay


newtype Link = Link { url :: Text }
  deriving (Show, Generic)
instance FromJSON Link

newtype Init = Init { linkZone :: [Link] }
  deriving (Show, Generic)
instance FromJSON Init

newtype Zones = Zones { zoneSlotGrid :: Text }
  deriving (Show, Generic)
instance FromJSON Zones

data DailySchedule = DailySchedule
  { inits :: [Init] 
  , zones :: Zones
  }
  deriving (Show, Generic)
instance FromJSON DailySchedule

parseIds :: Text -> Maybe [Text]
parseIds response = do
  jsonResponse <- decode' $ BL.fromStrict $ encodeUtf8 response
  h <- head $ inits jsonResponse
  let res = mapMaybe (head . getAllTextSubmatches . matches . T.unpack . url) $ linkZone h
  return $ map T.pack res
    where 
      re = "[0-9][0-9]+$" :: String
      matches x = (x :: String) =~ re :: AllTextSubmatches [] String

getDay :: Int -> Crawl [Text] 
getDay day = do
  $(logDebug) ("getDay " <> show day)
  resp <- postText dayUrl headers httpData
  case parseIds resp of
    Nothing -> return []
    Just res -> return res

    where
      dayUrl = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.changejour/" <> show day
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"

selectDay :: Text -> Crawl ()
selectDay slotId = do
  $(logDebug) ""
  $(logDebug) ("selectDay " <> slotId)

  _ <- takeWhileM (not . elem slotId) (map getDay [0..6])

  return ()

selectSchedule :: Text -> Crawl ()
selectSchedule slotId = do
  $(logDebug) ""
  $(logDebug) $ "selectSchedule" <> slotId
  _ <- selectDay slotId
  _ <- postText scheduleUrl headers httpData

  return ()
    where
      scheduleUrl = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.selectslot/" <> slotId
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"



shopListSel :: Selector
shopListSel = "li" @: [hasClass "slotHoraire-listeHeureItem"]

entryShopLink :: Scraper Text [Maybe ParsedSlotInfo]
entryShopLink = chroots shopListSel (parseAvailableSlot anySelector <|> parsePastSlot anySelector)

parsePastSlot :: Selector -> Scraper Text (Maybe ParsedSlotInfo)
parsePastSlot _ = do
  t <- text $ "span" @: [hasClass "slotHoraire-listeHeureLink"]
  let tod = parseTimeOrError False defaultTimeLocale "%lh%M" (T.unpack t)
  return $ Just (Nothing, tod, Past)

parseAvailableSlot :: Selector -> Scraper Text (Maybe ParsedSlotInfo)
parseAvailableSlot _ = do
  t <- text $ "a" @: [hasClass "slotHoraire-listeHeureLink"]
  let tod = parseTimeOrError False defaultTimeLocale "%lh%M" (T.unpack t)

  href <- attr "href" $ "a" @: [hasClass "slotHoraire-listeHeureLink"]
  let id = head $ getAllTextSubmatches $ matches $ T.unpack href

  case id of
    Nothing -> return Nothing
    Just id2 -> return $ Just (Just $ T.pack id2, tod, Available)

    where 
      re = "[0-9][0-9]+$" :: String
      matches x = (x :: String) =~ re :: AllTextSubmatches [] String





parseIds2 :: Text -> Integer -> Maybe [SlotInfo]
parseIds2 response day = do

  jsonResponse <- decode' $ BL.fromStrict $ encodeUtf8 response :: Maybe DailySchedule
  let tags = parseTags $ zoneSlotGrid $ zones jsonResponse

  slotInfo <- scrape entryShopLink tags
  let slots = map (makeSlotInfo day) $ catMaybes slotInfo 

  return slots

getDay2 :: Integer -> Crawl [SlotInfo] 
getDay2 day = do

  $(logDebug) ("getDay2 " <> show day)
  resp <- postText dayUrl headers httpData

  -- d <- getCurrentTime >>= toGregorian . utctDay
  -- $(logDebug) (show d)
  
  case parseIds2 resp day of
    Nothing -> return []
    Just res -> do
      $(logDebug) (show res)
      return res

    where
      dayUrl = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.changejour/" <> show day <> ""
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"


getSchedule :: Crawl [SlotInfo]
getSchedule = do
  $(logDebug) ""
  $(logDebug) $ "getSchedule"

  slotsByDay <- mapM getDay2 [0..6]

  return $ concat slotsByDay
