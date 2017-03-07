{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Drive.Crawl.Auchan.Schedule (selectSchedule) where

import           Protolude       hiding (Selector, inits)
import           Prelude                    (String)
import           Drive.Crawl
import           Drive.Utils
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text       as T
import           Data.Aeson
import           Text.Regex.TDFA


newtype Link = Link { url :: Text }
  deriving (Show, Generic)
instance FromJSON Link

newtype Init = Init { linkZone :: [Link] }
  deriving (Show, Generic)
instance FromJSON Init

newtype DailySchedule = DailySchedule { inits :: [Init] }
  deriving (Show, Generic)
instance FromJSON DailySchedule

parseIds :: Text -> Maybe [Text]
parseIds response = do
  jsonResponse <- decode' $ BL.fromStrict $ encodeUtf8 response
  h <- head $ inits jsonResponse
  let res = catMaybes $ map (head . getAllTextSubmatches . matches . T.unpack . url) $ linkZone h
  return $ map T.pack res
    where 
      re = ("[0-9][0-9]+$" :: String)
      matches x = (x :: String) =~ re :: AllTextSubmatches [] String

checkDay :: Int -> Crawl [Text] 
checkDay day = do
  $(logDebug) ("checkDay " <> show day)
  resp <- postText dayUrl headers httpData
  case parseIds resp of
    Nothing -> return []
    Just res -> return res

    where
      dayUrl = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.changejour/" <> show day
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"

getDay :: Text -> Crawl ()
getDay slotId = do
  $(logDebug) ""
  $(logDebug) ("getDay " <> slotId)

  _ <- takeWhileM (not . elem slotId) (map checkDay [0..10])

  return ()

selectSchedule :: Text -> Crawl ()
selectSchedule slotId = do
  $(logDebug) ""
  $(logDebug) $ "selectSchedule" <> slotId
  _ <- getDay slotId
  _ <- postText scheduleUrl headers httpData

  return ()
    where
      scheduleUrl = "https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.selectslot/" <> slotId
      headers = [("X-Requested-With", "XMLHttpRequest")]
      httpData = "t%3Azoneid=forceAjax"
