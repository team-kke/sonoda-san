module Line.Messaging.Webhook.Types (
  module Line.Messaging.Common.Types,
  WebhookResult (..),
  WebhookFailure (..),
  ReplyToken,
  Body (..),
  Event (..),
  EventSource (..),
  IDed (..),
  IncomingMessage (..),
  BeaconData (..),
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Line.Messaging.API.Types
import Line.Messaging.Common.Types
import Network.Wai (Response, Application)
import qualified Data.Text as T

data WebhookResult = Ok
                   | WaiResponse Response
                   | WaiApp Application

data WebhookFailure = SignatureVerificationFailed
                    | MessageDecodeFailed
                    deriving Show

type ReplyToken = T.Text

-- The Event data type and instances for proper type classes (e.g. FromJson)
-- should be implemented here.
-- For the Event spec, please refer to the official doc.
--
-- https://devdocs.line.me/en/#webhook-event-object

newtype Body = Body [Event]

instance FromJSON Body where
  parseJSON (Object v) = Body <$> v .: "events"
  parseJSON _ = fail "Body"

data Event = MessageEvent EventSource UTCTime ReplyToken IncomingMessage
           | FollowEvent EventSource UTCTime ReplyToken
           | UnfollowEvent EventSource UTCTime
           | JoinEvent EventSource UTCTime ReplyToken
           | LeaveEvent EventSource UTCTime
           | PostbackEvent EventSource UTCTime ReplyToken T.Text
           | BeaconEvent EventSource UTCTime ReplyToken BeaconData
           deriving Show

parseCommon :: (EventSource -> UTCTime -> a) -> Object -> Parser a
parseCommon f v = f <$> (v .: "source")
                    <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> v .: "timestamp")

withReplyToken :: Parser (T.Text -> a) -> Object -> Parser a
withReplyToken p v = p <*> v .: "replyToken"

instance FromJSON Event where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
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
  parseJSON _ = fail "Event"

data EventSource = User ID
                 | Group ID
                 | Room ID
                 deriving Show

instance FromJSON EventSource where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "user" -> User . ID <$> v .: "userId"
      "group" -> Group . ID <$> v .: "groupId"
      "room" -> Room . ID <$> v .: "roomId"
      _ -> fail "EventSource"
  parseJSON _ = fail "EventSource"

data IDed a = IDed ID a
            deriving Show

instance FromJSON a => FromJSON (IDed a) where
  parseJSON v = IDed <$> parseJSON v <*> parseJSON v

data IncomingMessage = TextIM (IDed Text)
                     | ImageIM ID
                     | VideoIM ID
                     | AudioIM ID
                     | LocationIM (IDed Location)
                     | StickerIM (IDed Sticker)
                     deriving Show

instance FromJSON IncomingMessage where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "text" -> TextIM <$> parseJSON (Object v)
      "image" -> ImageIM <$> parseJSON (Object v)
      "video" -> VideoIM <$> parseJSON (Object v)
      "audio" -> AudioIM <$> parseJSON (Object v)
      "location" -> LocationIM <$> parseJSON (Object v)
      "sticker" -> StickerIM <$> parseJSON (Object v)
      _ -> fail "IncomingMessage"
  parseJSON _ = fail "IncommingMessage"

data BeaconData = BeaconEnter { hwid :: ID }
                deriving Show

instance FromJSON BeaconData where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "enter" -> BeaconEnter . ID <$> v .: "hwid"
      _ -> fail "BeaconData"
  parseJSON _ = fail "BeaconData"
