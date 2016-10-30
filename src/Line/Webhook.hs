module Line.Webhook
  ( webhookApp
  , module Line.Webhook.Types
  ) where

import Line.Webhook.Types
import Network.HTTP.Types.Status
import Network.Wai

webhookApp :: ([Event] -> IO Result) -> Application
webhookApp webhook req f = do
  -- FIXME
  result <- webhook []
  f $ responseBuilder status200 [] "ok"
