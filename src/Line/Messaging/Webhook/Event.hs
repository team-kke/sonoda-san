module Line.Messaging.Webhook.Event (
  Body (..),
  Event (..),
  EventSource (..),
  MessageData (..),
  BeaconData (..),
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
                          , message :: MessageData
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
                   <*> v .: "message"
      "follow" -> parseCommon FollowEvent v `withReplyToken` v
      "unfollow" -> parseCommon UnfollowEvent v
      "join" -> parseCommon JoinEvent v `withReplyToken` v
      "leave" -> parseCommon LeaveEvent v
      "postback" -> parseCommon PostbackEvent v `withReplyToken` v
                    <*> ((v .: "postback") >>= (.: "data"))
      "beacon" -> parseCommon BeaconEvent v `withReplyToken` v
                  <*> v .: "beacon"
      _ -> fail "Event"

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
      _ -> fail "EventSource"

data MessageData = TextMessage { id :: Text
                               , text :: Text
                               }
                 | ImageMessage { id :: Text }
                 | VideoMessage { id :: Text }
                 | AudioMessage { id :: Text }
                 | LocationMessage { id :: Text
                                   , title :: Text
                                   , address :: Text
                                   , latitude :: Double
                                   , longitude :: Double
                                   }
                 | StickerMessage { id :: Text
                                  , packageId :: Text
                                  , stickerId :: Text
                                  }
                 deriving Show

parseId :: (Text -> a) -> Object -> Parser a
parseId f v = f <$> (v .: "id")

instance FromJSON MessageData where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "text" -> parseId TextMessage v <*> v .: "text"
      "image" -> parseId ImageMessage v
      "video" -> parseId VideoMessage v
      "audio" -> parseId AudioMessage v
      "location" -> parseId LocationMessage v
                    <*> v .: "title"
                    <*> v .: "address"
                    <*> v .: "latitude"
                    <*> v .: "longitude"
      "sticker" -> parseId StickerMessage v
                          <*> v .: "packageId"
                          <*> v .: "stickerId"
      _ -> fail "MessageData"

data BeaconData = BeaconEnter { hwid :: Text }
                deriving Show

instance FromJSON BeaconData where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "enter" -> BeaconEnter <$> v .: "hwid"
      _ -> fail "BeaconData"
