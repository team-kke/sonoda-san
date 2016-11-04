module Server.Webhook (
  app,
  ) where

import Config (getChannelSecret)
import Line.Webhook (webhookApp, Event, WebhookResult(..), defaultOnFailure)
import Network.Wai

app :: Application
app req f = do
  secret <- getChannelSecret
  webhookApp secret handler defaultOnFailure req f

handler :: [Event] -> IO WebhookResult
handler _ = return Ok
