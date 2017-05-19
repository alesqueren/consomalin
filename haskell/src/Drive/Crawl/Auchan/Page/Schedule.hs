{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Schedule (load, makeSlot, getSchedule, selectSchedule) where

import           Protolude       hiding (Selector, inits)
import           Prelude                    (String)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text       as T
import           Data.Aeson
import           Text.Regex.TDFA hiding (extract)
import           Text.HTML.TagSoup

import           Drive.Crawl
import           Drive.Slot
import           Drive.Attendance

import Data.Time
import Data.Time.Calendar.WeekDate

data SlotInfo = SlotInfo 
  { sId :: Maybe Text
  , sDayFromNow :: Integer
  , sTime :: TimeOfDay
  , sStatus :: SlotStatus
  }
  deriving (Show, Generic)
instance ToJSON SlotInfo

type ParsedSlotInfo = (Maybe Text, TimeOfDay, SlotStatus)

makeSlotInfo :: Integer -> ParsedSlotInfo -> SlotInfo
makeSlotInfo d (i,t,s) = SlotInfo i d t s

makeSlot :: Attendance -> Day -> SlotInfo -> Slot
makeSlot att currDay si =
  Slot (sId si) d t (sStatus si) (mongoGet d t att)
    where 
      t = sTime si
      d = skipSunday currDay (sDayFromNow si)

skipSunday :: Day -> Integer -> Day
skipSunday currDay n =
  if wd + fromIntegral n >= 7
    then addDays (n+1) currDay
    else addDays n currDay
  where
    (_,_,wd) = toWeekDate currDay


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

load :: Crawl [Tag Text]
load = requestTag $ Req url "GET" [] ""
  where url = "https://www.auchandrive.fr/drive/coffre.basketsummary.finalisercoffre"

loadDay :: Integer -> Crawl BL.ByteString
loadDay dayNb = do
  $(logDebug) ("getDay " <> show dayNb)
  requestJson $ Req dayUrl "POST" headers httpData
  where
    dayUrl = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.changejour/" <> show dayNb
    headers = [("X-Requested-With", "XMLHttpRequest")]
    httpData = "t%3Azoneid=forceAjax"


parseIds :: BL.ByteString -> Maybe [Text]
parseIds resp = do
  jsonResponse <- decode' resp
  h <- head $ inits jsonResponse
  let res = mapMaybe (head . getAllTextSubmatches . matches . T.unpack . url) $ linkZone h
  return $ map T.pack res
    where 
      re = "[0-9][0-9]+$" :: String
      matches x = (x :: String) =~ re :: AllTextSubmatches [] String

getDay :: Integer -> Crawl [Text] 
getDay dayNb = do
  $(logDebug) ("getDay " <> show dayNb)
  resp <- loadDay dayNb

  case parseIds resp of
    Nothing -> return []
    Just res -> return res


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
  -- _ <- postText scheduleUrl headers httpData
  resp <- requestTag $ Req url "POST" headers httpData

  return ()
    where
      url = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.selectslot/" <> slotId
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"



shopListSel :: Selector
shopListSel = "li" @: [hasClass "slotHoraire-listeHeureItem"]

-- TODO: EXTRACT SELECTORS !
entryShopLink :: Scraper Text [Maybe ParsedSlotInfo]
entryShopLink = chroots shopListSel (parseAvailableSlot <|> parsePastSlot)

parsePastSlot :: Scraper Text (Maybe ParsedSlotInfo)
parsePastSlot = do
  t <- text $ "span" @: [hasClass "slotHoraire-listeHeureLink"]
  let tod = parseTimeOrError False defaultTimeLocale "%lh%M" (T.unpack t)
  return $ Just (Nothing, tod, Past)

parseAvailableSlot :: Scraper Text (Maybe ParsedSlotInfo)
parseAvailableSlot = do
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



extractSlotInfo :: Integer -> DailySchedule -> [SlotInfo]
extractSlotInfo dayNb ds =
  map (makeSlotInfo dayNb) $
    catMaybes $
      fromMaybe [] $
        scrape entryShopLink $ parseTags $ 
          zoneSlotGrid $ zones ds

getSchedule :: Crawl [SlotInfo]
getSchedule = do
  $(logDebug) ""
  $(logDebug) $ "getSchedule"

  extSlots <- mapM (\d -> extractFromJson (extractSlotInfo d) $ loadDay d) [0..6]

  return $ concat extSlots
