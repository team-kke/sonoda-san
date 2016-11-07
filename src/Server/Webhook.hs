module Server.Webhook (
  app,
  ) where

import Config (getChannelSecret, getChannelAccessToken)
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (runReaderT)
import Line.Messaging.API (runAPI, reply)
import Line.Messaging.Webhook
import Network.Wai
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

app :: Application
app req f = do
  secret <- getChannelSecret
  webhookApp secret handler defaultOnFailure req f

handler :: [Event] -> IO WebhookResult
handler events = do
  forM_ events handleEvent
  return Ok

handleEvent :: Event -> IO ()
handleEvent (MessageEvent source _ replyToken (TextMessage _ text))
  | "園田さん、" `T.isPrefixOf` text = do
      let echo = T.drop 5 text
      print source
      runAPI getChannelAccessToken $ do
        reply replyToken echo
  | otherwise = return ()
handleEvent _ = return ()
