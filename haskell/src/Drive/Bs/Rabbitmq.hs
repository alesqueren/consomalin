{-# LANGUAGE DeriveGeneric #-}

module Drive.Bs.Rabbitmq (RabbitmqResource(..), 
                          RegistrationMessage(..), 
                          listen) where

import           Protolude hiding (msg)
import           Network.AMQP
import           Utils.Env
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import           Data.Aeson
import           GHC.Generics (Generic)

newtype RegistrationMessage = RegistrationMessage { user :: Text }
  deriving (Typeable, Show, Eq, Generic)
instance FromJSON RegistrationMessage
instance ToJSON RegistrationMessage

data RabbitmqResource = RegistrationResource

getQueueName :: RabbitmqResource -> Text
getQueueName RegistrationResource = "registrations"


listen ::(FromJSON m) => RabbitmqResource -> (m -> IO ()) -> IO ()
listen r cb = do
  h <- fromEnvOr "AMQP_HOST" A.takeText "127.0.0.1"
  u <- fromEnvOr "AMQP_USER" A.takeText "guest"
  p <- fromEnvOr "AMQP_PASS" A.takeText "guest"

  conn <- openConnection (T.unpack h) "/" u p
  chan <- openChannel conn

  _ <- declareQueue chan newQueue {queueName = qname}

  _ <- consumeMsgs chan qname Ack $ processMsg cb
  putStrLn ("listening to " <> qname <> "...":: Text)

  -- closeConnection conn
  -- putStrLn ("connection closed" :: Text)

  where
    qname = getQueueName r

processMsg :: (FromJSON a) => (a -> IO ()) -> (Message,Envelope) -> IO ()
processMsg cb (msg, env) = do
  putStrLn $ "received message: " <> msgBody msg
  case decode (msgBody msg) of
    Just tm -> cb tm
    Nothing -> putStrLn ("parsing error" :: Text)
  ackEnv env
