{-# LANGUAGE DeriveGeneric #-}

module Drive.Bs.Rabbitmq (RabbitmqResource(..), TransactionMessage(..), listen) where

import           Protolude hiding (msg)
import           Network.AMQP
import           Drive.Utils
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import           Data.Aeson
import           GHC.Generics (Generic)

data TransactionMessage = TransactionMessage 
  { user :: !Text 
  , transaction :: !Text 
  }
  deriving (Typeable, Show, Eq, Generic)
instance FromJSON TransactionMessage
instance ToJSON TransactionMessage

data RabbitmqResource = TransactionResource

getQueueName :: RabbitmqResource -> Text
getQueueName TransactionResource = "transactions"


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
