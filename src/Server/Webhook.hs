module Server.Webhook (
  app,
  ) where

import Line.Webhook (webhookApp, Event, WebhookResult(..), defaultOnFailure)
import Network.Wai

app :: Application
app = webhookApp handler defaultOnFailure

handler :: [Event] -> IO WebhookResult
handler _ = return Ok
