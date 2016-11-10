module Server (
  app,
  ) where

import Config (getChannelSecret, getChannelAccessToken)
import Control.Monad (forM_)
import Line.Messaging.API (runAPI, APIIO, reply, OutgoingMessage(..), Text(..))
import Line.Messaging.Webhook hiding (webhook)
import Network.Wai
import Response (response404)
import qualified Data.Text as T

app :: Application
app req f =
  case pathInfo req of
    "webhook" : _ -> webhook req f
    _ -> f response404

webhook :: Application
webhook req f = do
  secret <- getChannelSecret
  webhookApp secret handler defaultOnFailure req f

handler :: [Event] -> IO WebhookResult
handler events = do
  forM_ events handleEvent
  return Ok

api :: APIIO a -> IO a
api = runAPI getChannelAccessToken

handleEvent :: Event -> IO ()
handleEvent (MessageEvent event) = case message event of
  TextIM (IDed _ (Text text)) -> handleText event text
  LocationIM (IDed _ location) -> do
    print location
    api $ reply (replyToken event) [LocationOM location, TextOM $ Text "どこですか？"]
  StickerIM (IDed _ sticker) -> do
    api $ reply (replyToken event) [StickerOM sticker]
  _ -> return ()
handleEvent _ = return ()

handleText :: ReplyableMessage IncomingMessage -> T.Text -> IO ()
handleText event text
  | "園田さん、" `T.isPrefixOf` text = do
      print $ source event
      api $ reply (replyToken event) [TextOM $ Text $ T.drop 5 text]
  | otherwise = return ()
