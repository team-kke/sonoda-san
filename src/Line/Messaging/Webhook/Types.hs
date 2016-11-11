module Line.Messaging.Webhook.Types (
  module Line.Messaging.Common.Types,
  WebhookResult (..),
  WebhookFailure (..),
  ReplyToken,
  Body (..),
  Event (..),
  ReplyableEvent,
  NonReplyableEvent,
  MessageEvent,
  FollowEvent,
  UnfollowEvent,
  JoinEvent,
  LeaveEvent,
  PostbackEvent,
  BeaconEvent,
  EventCommon,
  source,
  datetime,
  replyToken,
  message,
  postback,
  beacon,
  EventSource (..),
  identifier,
  IncomingMessage (..),
  IDed (..),
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
                    deriving (Eq, Show)

type ReplyToken = T.Text

-- The Event data type and instances for proper type classes (e.g. FromJson)
-- should be implemented here.
-- For the Event spec, please refer to the official doc.
--
-- https://devdocs.line.me/en/#webhook-event-object

newtype Body = Body [Event]
             deriving (Eq, Show)

instance FromJSON Body where
  parseJSON (Object v) = Body <$> v .: "events"
  parseJSON _ = fail "Body"

data ReplyableEvent a = ReplyableEvent EventSource UTCTime ReplyToken a deriving (Eq, Show)
data NonReplyableEvent a = NonReplyableEvent EventSource UTCTime a deriving (Eq, Show)

type MessageEvent = ReplyableEvent IncomingMessage
type FollowEvent = ReplyableEvent ()
type UnfollowEvent = NonReplyableEvent ()
type JoinEvent = ReplyableEvent ()
type LeaveEvent = NonReplyableEvent ()
type PostbackEvent = ReplyableEvent T.Text
type BeaconEvent = ReplyableEvent BeaconData

class EventCommon m where
  source :: m -> EventSource
  datetime :: m -> UTCTime

instance EventCommon (ReplyableEvent a) where
  source (ReplyableEvent x _ _ _) = x
  datetime (ReplyableEvent _ x _ _) = x

instance EventCommon (NonReplyableEvent a) where
  source (NonReplyableEvent x _ _) = x
  datetime (NonReplyableEvent _ x _) = x

replyToken :: ReplyableEvent a -> ReplyToken
replyToken (ReplyableEvent _ _ x _) = x

message :: MessageEvent -> IncomingMessage
message (ReplyableEvent _ _ _ x) = x

postback :: PostbackEvent -> T.Text
postback (ReplyableEvent _ _ _ x) = x

beacon :: BeaconEvent -> BeaconData
beacon (ReplyableEvent _ _ _ x) = x

data Event = MessageEvent MessageEvent
           | FollowEvent FollowEvent
           | UnfollowEvent UnfollowEvent
           | JoinEvent JoinEvent
           | LeaveEvent LeaveEvent
           | PostbackEvent PostbackEvent
           | BeaconEvent BeaconEvent
           deriving (Eq, Show)

parseCommon :: (EventSource -> UTCTime -> a) -> Object -> Parser a
parseCommon f v = f <$> (v .: "source")
                    <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> v .: "timestamp")

withReplyToken :: Parser (T.Text -> a) -> Object -> Parser a
withReplyToken p v = p <*> v .: "replyToken"

instance FromJSON Event where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "message" -> MessageEvent <$> (parseCommon ReplyableEvent v `withReplyToken` v
                                      <*> v .: "message")
      "follow" -> FollowEvent <$> (parseCommon ReplyableEvent v `withReplyToken` v
                                    <*> return ())
      "unfollow" -> UnfollowEvent <$> (parseCommon NonReplyableEvent v
                                        <*> return ())
      "join" -> JoinEvent <$> (parseCommon ReplyableEvent v `withReplyToken` v
                                <*> return ())
      "leave" -> LeaveEvent <$> (parseCommon NonReplyableEvent v
                                  <*> return ())
      "postback" -> PostbackEvent <$> (parseCommon ReplyableEvent v `withReplyToken` v
                                        <*> ((v .: "postback") >>= (.: "data")))
      "beacon" -> BeaconEvent <$> (parseCommon ReplyableEvent v `withReplyToken` v
                                    <*> v .: "beacon")
      _ -> fail "Event"
  parseJSON _ = fail "Event"

data EventSource = User ID
                 | Group ID
                 | Room ID
                 deriving (Eq, Show)

identifier :: EventSource -> ID
identifier (User i) = i
identifier (Group i) = i
identifier (Room i) = i

instance FromJSON EventSource where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "user" -> User . ID <$> v .: "userId"
      "group" -> Group . ID <$> v .: "groupId"
      "room" -> Room . ID <$> v .: "roomId"
      _ -> fail "EventSource"
  parseJSON _ = fail "EventSource"

data IDed a = IDed ID a
            deriving (Eq, Show)

instance FromJSON a => FromJSON (IDed a) where
  parseJSON v = IDed <$> parseJSON v <*> parseJSON v

data IncomingMessage = TextIM (IDed Text)
                     | ImageIM ID
                     | VideoIM ID
                     | AudioIM ID
                     | LocationIM (IDed Location)
                     | StickerIM (IDed Sticker)
                     deriving (Eq, Show)

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
                deriving (Eq, Show)

instance FromJSON BeaconData where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "enter" -> BeaconEnter . ID <$> v .: "hwid"
      _ -> fail "BeaconData"
  parseJSON _ = fail "BeaconData"
