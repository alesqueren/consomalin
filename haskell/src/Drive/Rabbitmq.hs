{-# LANGUAGE DeriveGeneric #-}

module Drive.Rabbitmq (RabbitmqResource(..), listen) where

import           Protolude hiding (msg)
import           Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import           Drive.Utils
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import           Data.Aeson
import           GHC.Generics (Generic)
import           Drive.User
import           Drive.Crawl.Auchan

newtype TransactionMessage = TransactionMessage { user :: Text }
  deriving (Typeable, Show, Eq, Generic)

instance FromJSON TransactionMessage
instance ToJSON TransactionMessage

processTransactionMessage :: TransactionMessage -> IO ()
processTransactionMessage tm = do
  mt <- findTransaction $ user tm
  case mt of
    Just t -> makeTransaction t
    Nothing -> putStrLn ("Error: no transaction found" :: Text)

data RabbitmqResource = TransactionResource

class Queryable a where
  getQueue :: a -> Text
  -- TODO: no TransactionMessage (rigid type)
  getCallback :: a -> (TransactionMessage -> IO ())

instance Queryable RabbitmqResource where
  getQueue TransactionResource = "transactions"
  getCallback TransactionResource = processTransactionMessage


listen ::(Queryable r) => r -> IO ()
listen r = do
  h <- fromEnvOr "AMQP_HOST" A.takeText "127.0.0.1"
  u <- fromEnvOr "AMQP_USER" A.takeText "guest"
  p <- fromEnvOr "AMQP_PASS" A.takeText "guest"

  conn <- openConnection (T.unpack h) "/" u p
  chan <- openChannel conn

  declareQueue chan newQueue {queueName = qname}

  consumeMsgs chan qname Ack $ processMsg $ getCallback r
  putStrLn ("listening to " <> qname <> "...":: Text)

  -- closeConnection conn
  -- putStrLn ("connection closed" :: Text)

  where
    qname = getQueue r

processMsg :: (FromJSON a) => (a -> IO ()) -> (Message,Envelope) -> IO ()
processMsg cb (msg, env) = do
  putStrLn $ "received message: " ++ BL.unpack (msgBody msg)
  case decode (msgBody msg) of
    Just tm -> cb tm
    Nothing -> putStrLn ("parsing error" :: Text)
  ackEnv env
