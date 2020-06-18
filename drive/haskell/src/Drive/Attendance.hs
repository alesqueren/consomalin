{-# LANGUAGE FlexibleContexts    #-}

module Drive.Attendance (Attendance(..), mongoFind, getLevel) where

import           Protolude
import           Database.MongoDB
import           Network.CGI.Protocol
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Data.Text       as T
import qualified Data.Map.Strict as M

import           Drive.Bs.Mongo

newtype Attendance = Attendance (Map Int (Map TimeOfDay Float))
  deriving (Typeable, Show, Eq)

instance Val Attendance where
  -- val a = val []
  cast' (Doc doc) =
    return $ Attendance $ M.fromList $ mapMaybe castDay doc
      where
        castDay :: Field -> Maybe (Int, M.Map TimeOfDay Float)
        castDay field = do
          l <- maybeRead $ T.unpack $ label field
          v <- (cast' . value) field
          return (l, M.fromList $ mapMaybe castHour v)
        castHour :: Field -> Maybe (TimeOfDay,Float)
        castHour field = do
          l <- parseTimeM True defaultTimeLocale "%H:%M:%S" $ T.unpack $ label field
          v <- (cast' . value) field
          return (l, v)
  cast' _ = Nothing

mongoFind :: Text -> IO (Maybe Attendance)
mongoFind s = doSelectOne AttendanceResource ["_id" =: s]

getLevel :: Day -> TimeOfDay -> Attendance -> Maybe Float
getLevel d t (Attendance m) = do
  day <- M.lookup dayOfWeek m
  let a = M.mapWithKey (\k v -> (abs (timeOfDayToTime k - timeOfDayToTime t), v)) day
  let (_,res) = minimumBy (\(k1,_) (k2,_) -> compare k1 k2) a
  return res
    where (_,_,dayOfWeek) = toWeekDate d
