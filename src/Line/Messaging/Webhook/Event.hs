module Line.Messaging.Webhook.Event (
  Body (..),
  Event (..),
  ID,
  EventSource (..),
  Message (..),
  BeaconData (..),
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Line.Types (ReplyToken, ID)

-- The Event data type and instances for proper type classes (e.g. FromJson)
-- should be implemented here.
-- For the Event spec, please refer to the official doc.
--
-- https://devdocs.line.me/en/#webhook-event-object

newtype Body = Body { events :: [Event] }

instance FromJSON Body where
  parseJSON (Object v) = Body <$> v .: "events"

data Event = MessageEvent EventSource UTCTime ReplyToken Message
           | FollowEvent EventSource UTCTime ReplyToken
           | UnfollowEvent EventSource UTCTime
           | JoinEvent EventSource UTCTime ReplyToken
           | LeaveEvent EventSource UTCTime
           | PostbackEvent EventSource UTCTime ReplyToken Text
           | BeaconEvent EventSource UTCTime ReplyToken BeaconData
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

data EventSource = User ID
                 | Group ID
                 | Room ID
                 deriving Show

instance FromJSON EventSource where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "user" -> User <$> v .: "userId"
      "group" -> Group <$> v .: "groupId"
      "room" -> Room <$> v .: "roomId"
      _ -> fail "EventSource"


data Location = Location { title :: Text
                         , address :: Text
                         , latitude :: Double
                         , longitude :: Double
                         }
                deriving Show

data Sticker = Sticker { package :: ID
                       , sticker :: ID
                       }
               deriving Show

data Message = TextMessage ID Text
             | ImageMessage ID
             | VideoMessage ID
             | AudioMessage ID
             | LocationMessage ID Location
             | StickerMessage ID Sticker
             deriving Show

parseId :: (Text -> a) -> Object -> Parser a
parseId f v = f <$> (v .: "id")

withLocation :: Parser (Location -> a) -> Object -> Parser a
withLocation p v = p <*> (Location <$> v .: "title"
                                   <*> v .: "address"
                                   <*> v .: "latitude"
                                   <*> v .: "longitude")

withSticker :: Parser (Sticker -> a) -> Object -> Parser a
withSticker p v = p <*> (Sticker <$> v .: "packageId"
                                 <*> v .: "stickerId")

instance FromJSON Message where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "text" -> parseId TextMessage v <*> v .: "text"
      "image" -> parseId ImageMessage v
      "video" -> parseId VideoMessage v
      "audio" -> parseId AudioMessage v
      "location" -> parseId LocationMessage v `withLocation` v
      "sticker" -> parseId StickerMessage v `withSticker` v
      _ -> fail "Message"

data BeaconData = BeaconEnter { hwid :: Text }
                deriving Show

instance FromJSON BeaconData where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: Text of
      "enter" -> BeaconEnter <$> v .: "hwid"
      _ -> fail "BeaconData"
