module Server (
  app,
  ) where

import Config (getChannelSecret, getChannelAccessToken)
import Control.Monad (forM_)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Line.Messaging.API
import Line.Messaging.Webhook hiding (webhook)
import Network.Wai
import Response (response200, response404)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

app :: Application
app req f =
  case pathInfo req of
    "webhook" : _ -> webhook req f
    "send" : id' : str : _ -> send id' str req f
    _ -> f response404

webhook :: Application
webhook req f = do
  secret <- getChannelSecret
  webhookApp secret handler defaultOnFailure req f

handler :: [Event] -> IO WebhookResult
handler events = do
  forM_ events handleEvent
  return Ok

api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelAccessToken

handleEvent :: Event -> IO ()
handleEvent (MessageEvent event) = case getMessage event of
  TextEM _ (Text text) -> handleText (getSource event) (getReplyToken event) text
  ImageEM id' -> downloadContent id' ".jpg"
  VideoEM id' -> downloadContent id' ".mp4"
  LocationEM  _ location -> do
    print location
    _ <- api $ reply (getReplyToken event) [Message location, Message $ Text "どこですか？"]
    return ()
  _ -> return ()
handleEvent _ = return ()

handleText :: EventSource -> ReplyToken -> T.Text -> IO ()
handleText source replyToken text
  | "園田さん、プッシュ" `T.isPrefixOf` text = do
      let m = T.concat [ "https://karen.noraesae.net/send/"
                       , getId source
                       , "/メッセージ"
                       ]
      _ <- api $ reply replyToken [Message $ Text m]
      return ()
  | "園田さん、" `T.isPrefixOf` text = do
      print source
      _ <- api $ reply replyToken [Message $ Text $ T.drop 5 text]
      return ()
  | otherwise = return ()

send :: ID -> T.Text -> Application
send id' str _ f = do
  _ <- api $ push id' [Message $ Text str]
  f $ response200 "ok"

downloadContent :: ID -> String -> IO ()
downloadContent id' ext = do
  c <- either (const "") id <$> (api $ getContent id')
  name <- (++ ext) . toString <$> nextRandom
  BL.writeFile name c
