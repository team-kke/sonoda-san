module Server.Webhook (
  app,
  ) where

import Config (getChannelSecret)
import Control.Monad (forM_)
import Line.Messaging.Webhook (webhookApp, Event, WebhookResult(..), defaultOnFailure)
import Network.Wai

app :: Application
app req f = do
  secret <- getChannelSecret
  webhookApp secret handler defaultOnFailure req f

handler :: [Event] -> IO WebhookResult
handler events = do
  forM_ events print
  return Ok
