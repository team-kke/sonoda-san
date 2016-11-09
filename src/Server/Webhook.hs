module Server.Webhook (
  app,
  ) where

import Config (getChannelSecret, getChannelAccessToken)
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (runReaderT)
import Line.Messaging.API (runAPI, reply)
import Line.Messaging.API.Types (Text (..))
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
handleEvent (MessageEvent source _ replyToken message) = case message of
  TextMessage (IDed _ (Text text)) -> do
    if "園田さん、" `T.isPrefixOf` text
      then do
        print source
        runAPI getChannelAccessToken $ do
          reply replyToken [Text $ T.drop 5 text]
      else return ()
handleEvent _ = return ()
