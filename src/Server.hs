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

api :: APIIO a -> IO a
api = runAPI getChannelAccessToken

handleEvent :: Event -> IO ()
handleEvent (MessageEvent event) = case message event of
  TextIM _ (Text text) -> handleText event text
  ImageIM id' -> downloadContent id' ".jpg"
  VideoIM id' -> downloadContent id' ".mp4"
  LocationIM  _ location -> do
    print location
    api $ reply (replyToken event) [Message location, Message $ Text "どこですか？"]
  _ -> return ()
handleEvent _ = return ()

handleText :: MessageEvent -> T.Text -> IO ()
handleText event text
  | "園田さん、プッシュ" `T.isPrefixOf` text = do
      let m = T.concat [ "https://karen.noraesae.net/send/"
                       , identifier . source $ event
                       , "/メッセージ"
                       ]
      api $ reply (replyToken event) [Message $ Text m]
  | "園田さん、" `T.isPrefixOf` text = do
      print $ source event
      api $ reply (replyToken event) [Message $ Text $ T.drop 5 text]
  | otherwise = return ()

send :: ID -> T.Text -> Application
send id' str _ f = do
  api $ push id' [Message $ Text str]
  f $ response200 "ok"

downloadContent :: ID -> String -> IO ()
downloadContent id' ext = do
  c <- api $ getContent id'
  name <- (++ ext) . toString <$> nextRandom
  BL.writeFile name  c
