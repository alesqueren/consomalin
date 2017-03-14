module Drive.Attendance (findAttendance) where

import           Protolude

import           Drive.Mongo
import           Database.MongoDB


-- type Attendance = [(Text,[(Text,Text)])]
data Attendance = Attendance [(Text,[(Text,Text)])]
  deriving (Typeable, Show, Eq)

instance Val Attendance where
  -- val a = val []
  cast' (Doc doc) = do
    return $ Attendance [] 

findAttendance :: Text -> IO Attendance
findAttendance s = 
  doSelectOne AttendanceResource ["_id" =: s]
