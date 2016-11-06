module Line.Webhook.Event (
  Body (..),
  Event (..),
  EventSource (..),
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- The Event data type and instances for proper type classes (e.g. FromJson)
-- should be implemented here.
-- For the Event spec, please refer to the official doc.
--
-- https://devdocs.line.me/en/#webhook-event-object

newtype Body = Body { events :: [Event] }

instance FromJSON Body where
  parseJSON (Object v) = Body <$> v .: "events"

data Event = MessageEvent { source :: EventSource
                          , datetime :: UTCTime
                          }
             deriving Show

parseCommon :: (EventSource -> UTCTime -> a) -> Object -> Parser a
parseCommon f v = f <$> (v .: "source")
                    <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> v .: "timestamp")

instance FromJSON Event where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      _ -> parseCommon MessageEvent v

data EventSource = User { userId :: Text }
                 | Group { groupId :: Text }
                 | Room { roomId :: Text }
                 deriving Show

instance FromJSON EventSource where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "user" -> User <$> v .: "userId"
      "group" -> Group <$> v .: "groupId"
      "room" -> Room <$> v .: "roomId"
