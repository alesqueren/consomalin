{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Page.Schedule (makeSlot, getSchedule, selectSchedule) where

import           Protolude       hiding (Selector, inits)
import           Prelude                    (String)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import           Data.Aeson
import           Text.Regex.TDFA hiding (extract)
import           Text.HTML.TagSoup
import           Control.Monad.Catch

import           Drive.Crawl
import           Drive.Slot
import           Drive.Attendance

import Data.Time
import Data.Time.Calendar.WeekDate

data SlotNotFoundException = SlotNotFoundException deriving (Show, Typeable)
instance Exception SlotNotFoundException

daysInSchedule :: Integer
daysInSchedule = 5

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

makeSlot :: Maybe Attendance -> Day -> SlotInfo -> Slot
makeSlot att currDay si =
  Slot (sId si) d t (sStatus si) level
    where 
      t = sTime si
      d = skipSunday currDay (sDayFromNow si)
      level = join $ fmap (getLevel d t) att

skipSunday :: Day -> Integer -> Day
skipSunday currDay n =
  if wd + fromIntegral n >= 7
    then addDays (n+1) currDay
    else addDays n currDay
  where
    (_,_,wd) = toWeekDate currDay

pastSlotInfoScraper :: EntityScraper ParsedSlotInfo
pastSlotInfoScraper = EntityScraper 
  { rootSelector = "li" @: [hasClass "slotHoraire-listeHeureItem"]
  , elementSelectors = M.fromList
    [ ("time", text $ "span" @: [hasClass "slotHoraire-listeHeureLink"]) ]
  , entityMaker = makePastSlot 
  }

slotInfoScraper :: EntityScraper ParsedSlotInfo
slotInfoScraper = EntityScraper 
  { rootSelector = "li" @: [hasClass "slotHoraire-listeHeureItem"]
  , elementSelectors = M.fromList
    [ ("time", text $ "a" @: [hasClass "slotHoraire-listeHeureLink"])
    , ("href", attr "href" $ "a" @: [hasClass "slotHoraire-listeHeureLink"])]
  , entityMaker = makeAvailableSlot 
  }


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
loadDay dayNb = 
  requestJson $ Req dayUrl "POST" headers httpData
  where
    dayUrl = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.changejour/" <> show dayNb
    headers = [("X-Requested-With", "XMLHttpRequest")]
    httpData = "t%3Azoneid=forceAjax"

confirmSlot :: Crawl [Tag Text] 
confirmSlot = requestTag $ Req url "GET" [] ""
  where url = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.chooseslot"


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
  $(logDebug) $ "getDay " <> show dayNb

  resp <- loadDay dayNb

  case parseIds resp of
    Nothing -> return []
    Just res -> return res


selectDay :: Text -> Crawl ()
selectDay slotId = do
  browsedDays <- takeWhileM (not . elem slotId) (map getDay [0..daysInSchedule])

  -- TODO: catch this exception
  when (length browsedDays == fromInteger (daysInSchedule + 1)) $
    throwM SlotNotFoundException

  return ()

selectSchedule :: Text -> Crawl ()
selectSchedule slotId = do
  $(logDebug) $ "selectSchedule " <> slotId
  _ <- load
  _ <- selectDay slotId
  _ <- requestTag $ Req url "POST" headers httpData
  _ <- confirmSlot
  return ()
    where
      url = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.selectslot/" <> slotId
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"


makePastSlot :: Map Text Text -> Maybe ParsedSlotInfo
makePastSlot m = do
  t <- M.lookup "time" m
  let tod = parseTimeOrError False defaultTimeLocale "%lh%M" (T.unpack t)
  return (Nothing, tod, Past)

makeAvailableSlot :: Map Text Text -> Maybe ParsedSlotInfo
makeAvailableSlot m = do
  t <- M.lookup "time" m
  let tod = parseTimeOrError False defaultTimeLocale "%lh%M" (T.unpack t)

  href <- M.lookup "href" m
  id <- head $ getAllTextSubmatches $ matches $ T.unpack href

  return (Just $ T.pack id, tod, Available)

  where 
    re = "[0-9][0-9]+$" :: String
    matches x = (x :: String) =~ re :: AllTextSubmatches [] String


extractSlotInfo :: Integer -> DailySchedule -> [SlotInfo]
extractSlotInfo dayNb ds =
  map (makeSlotInfo dayNb) $
    catMaybes $
      entityScrap pastSlotInfoScraper page
      ++
      entityScrap slotInfoScraper page

  where page = parseTags $ zoneSlotGrid $ zones ds

getSchedule :: Crawl [SlotInfo]
getSchedule = do
  $(logDebug) $ "getSchedule"
  _ <- load
  slotsByDay <- mapM (\d -> extractFromJson (extractSlotInfo d) $ loadDay d) [0..(daysInSchedule-1)]
  return $ concat slotsByDay
