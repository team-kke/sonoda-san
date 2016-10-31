module Line.Webhook (
  module Line.Webhook.Types,
  webhookApp,
  defaultOnFailure
  ) where

import Line.Webhook.Types
import Network.HTTP.Types.Status
import Network.Wai

webhookApp :: ([Event] -> IO WebhookResult)
           -> (WebhookFailure -> Application)
           -> Application
webhookApp eventHandler failureHandler req f = do
  -- FIXME
  result <- eventHandler []
  f $ responseBuilder status200 [] "ok"

defaultOnFailure :: WebhookFailure -> Application
defaultOnFailure = undefined
