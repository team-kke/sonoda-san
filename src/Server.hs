module Server (
  app,
  ) where

import Calculator (calc)
import Config (getChannelSecret, getChannelAccessToken)
import Control.Monad (forM_)
import Line.Messaging.API
import Line.Messaging.Webhook hiding (webhook)
import Network.Wai
import Response (response200, response404)
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

handler :: [Event] -> IO ()
handler events = forM_ events handleEvent

handleEvent :: Event -> IO ()
handleEvent (MessageEvent event) = case getMessage event of
  TextEM _ (Text text) -> do
    _ <- api $ handleText (getSource event) (getReplyToken event) text
    return ()
  _ -> return ()
handleEvent _ = return ()

handleText :: EventSource -> ReplyToken -> T.Text -> APIIO ()
handleText source replyToken text
  | "園田さん、プッシュ" `T.isPrefixOf` text = do
      let m = T.concat [ "https://karen.noraesae.net/send/"
                       , getID source
                       , "/こんにちは"
                       ]
      reply replyToken [Message . Text $ m]
  | "園田さん、" `T.isPrefixOf` text && "?" `T.isSuffixOf` text = do
      let expr = T.drop 5 . T.take (T.length text - 1) $ text
      reply replyToken [Message . Text $ calc expr]
  | "園田さん、" `T.isPrefixOf` text = do
      reply replyToken [Message . Text $ T.drop 5 text]
  | otherwise = return ()

send :: ID -> T.Text -> Application
send id' str _ f = do
  _ <- api $ push id' [Message $ Text str]
  f $ response200 "ok"

api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelAccessToken
