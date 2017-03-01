module Drive.Rabbitmq (RabbitmqResource(..), listen) where

import           Protolude hiding (msg)
import           Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import           Drive.Utils
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T


data RabbitmqResource = TransactionResource

class Queryable a where
  getQueue :: a -> Text

instance Queryable RabbitmqResource where
  getQueue TransactionResource = "transactions"

listen ::(Queryable r) => r -> IO ()
listen r = do
  h <- fromEnvOr "AMQP_HOST" A.takeText "127.0.0.1"
  u <- fromEnvOr "AMQP_USER" A.takeText "guest"
  p <- fromEnvOr "AMQP_PASS" A.takeText "guest"

  conn <- openConnection (T.unpack h) "/" u p
  chan <- openChannel conn

  declareQueue chan newQueue {queueName = qname}

  consumeMsgs chan qname Ack myCallback

  publishMsg chan "" qname
      newMsg {msgBody = BL.pack "hello world",
              msgDeliveryMode = Just Persistent}

  getLine -- wait for keypress
  closeConnection conn
  putStrLn ("connection closed" :: Text)

  where
    qname = getQueue r


myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
  putStrLn $ "received message: " ++ BL.unpack (msgBody msg)
  ackEnv env
