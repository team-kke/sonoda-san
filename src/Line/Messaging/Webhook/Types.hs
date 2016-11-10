module Line.Messaging.Webhook.Types (
  module Line.Messaging.Common.Types,
  WebhookResult (..),
  WebhookFailure (..),
  ReplyToken,
  Body (..),
  Event (..),
  ReplyableMessage,
  NonReplyableMessage,
  EventMessage,
  source,
  datetime,
  replyToken,
  message,
  postback,
  beacon,
  EventSource (..),
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

data ReplyableMessage a = ReplyableMessage EventSource UTCTime ReplyToken a deriving Show
data NonReplyableMessage a = NonReplyableMessage EventSource UTCTime a deriving Show

class EventMessage m where
  source :: m -> EventSource
  datetime :: m -> UTCTime

instance EventMessage (ReplyableMessage a) where
  source (ReplyableMessage x _ _ _) = x
  datetime (ReplyableMessage _ x _ _) = x

instance EventMessage (NonReplyableMessage a) where
  source (NonReplyableMessage x _ _) = x
  datetime (NonReplyableMessage _ x _) = x

replyToken :: ReplyableMessage a -> ReplyToken
replyToken (ReplyableMessage _ _ x _) = x

message :: ReplyableMessage IncomingMessage -> IncomingMessage
message (ReplyableMessage _ _ _ x) = x

postback :: ReplyableMessage T.Text -> T.Text
postback (ReplyableMessage _ _ _ x) = x

beacon :: ReplyableMessage BeaconData -> BeaconData
beacon (ReplyableMessage _ _ _ x) = x

data Event = MessageEvent (ReplyableMessage IncomingMessage)
           | FollowEvent (ReplyableMessage ())
           | UnfollowEvent (NonReplyableMessage ())
           | JoinEvent (ReplyableMessage ())
           | LeaveEvent (NonReplyableMessage ())
           | PostbackEvent (ReplyableMessage T.Text)
           | BeaconEvent (ReplyableMessage BeaconData)
           deriving Show

parseCommon :: (EventSource -> UTCTime -> a) -> Object -> Parser a
parseCommon f v = f <$> (v .: "source")
                    <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> v .: "timestamp")

withReplyToken :: Parser (T.Text -> a) -> Object -> Parser a
withReplyToken p v = p <*> v .: "replyToken"

instance FromJSON Event where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "message" -> MessageEvent <$> (parseCommon ReplyableMessage v `withReplyToken` v
                                      <*> v .: "message")
      "follow" -> FollowEvent <$> (parseCommon ReplyableMessage v `withReplyToken` v
                                    <*> return ())
      "unfollow" -> UnfollowEvent <$> (parseCommon NonReplyableMessage v
                                        <*> return ())
      "join" -> JoinEvent <$> (parseCommon ReplyableMessage v `withReplyToken` v
                                <*> return ())
      "leave" -> LeaveEvent <$> (parseCommon NonReplyableMessage v
                                  <*> return ())
      "postback" -> PostbackEvent <$> (parseCommon ReplyableMessage v `withReplyToken` v
                                        <*> ((v .: "postback") >>= (.: "data")))
      "beacon" -> BeaconEvent <$> (parseCommon ReplyableMessage v `withReplyToken` v
                                    <*> v .: "beacon")
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
