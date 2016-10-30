module Server
  ( app
  ) where

import Network.Wai
import Response (response404)
import qualified Server.Webhook as Webhook

app :: Application
app req f =
  case pathInfo req of
    "webhook" : _ -> Webhook.app req f
    _ -> f response404
