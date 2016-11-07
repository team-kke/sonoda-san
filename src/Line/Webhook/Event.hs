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
                          , replyToken :: Text
                          -- FIXME: message data
                          }
           | FollowEvent { source :: EventSource
                         , datetime :: UTCTime
                         , replyToken :: Text
                         }
           | UnfollowEvent { source :: EventSource
                           , datetime :: UTCTime
                           }
           | JoinEvent { source :: EventSource
                       , datetime :: UTCTime
                       , replyToken :: Text
                       }
           | LeaveEvent { source :: EventSource
                        , datetime :: UTCTime
                        }
           | PostbackEvent { source :: EventSource
                           , datetime :: UTCTime
                           , replyToken :: Text
                           , postback :: Text
                           }
           | BeaconEvent { source :: EventSource
                         , datetime :: UTCTime
                         , replyToken :: Text
                         , beacon :: BeaconData
                         }
           | InvalidEvent
           deriving Show

parseCommon :: (EventSource -> UTCTime -> a) -> Object -> Parser a
parseCommon f v = f <$> (v .: "source")
                    <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> v .: "timestamp")

withReplyToken :: Parser (Text -> a) -> Object -> Parser a
withReplyToken p v = p <*> v .: "replyToken"

instance FromJSON Event where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "message" -> parseCommon MessageEvent v `withReplyToken` v
      "follow" -> parseCommon FollowEvent v `withReplyToken` v
      "unfollow" -> parseCommon UnfollowEvent v
      "join" -> parseCommon JoinEvent v `withReplyToken` v
      "leave" -> parseCommon LeaveEvent v
      "postback" -> parseCommon PostbackEvent v `withReplyToken` v
                    <*> ((v .: "postback") >>= (.: "data"))
      "beacon" -> parseCommon BeaconEvent v `withReplyToken` v
                  <*> v .: "beacon"
      _ -> return InvalidEvent

data EventSource = User { userId :: Text }
                 | Group { groupId :: Text }
                 | Room { roomId :: Text }
                 | InvalidEventSource
                 deriving Show

instance FromJSON EventSource where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "user" -> User <$> v .: "userId"
      "group" -> Group <$> v .: "groupId"
      "room" -> Room <$> v .: "roomId"
      _ -> return InvalidEventSource

data BeaconData = BeaconEnter { hwid :: Text }
                | InvalidBeaconData
                deriving Show

instance FromJSON BeaconData where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "enter" -> BeaconEnter <$> v .: "hwid"
      _ -> return InvalidBeaconData
