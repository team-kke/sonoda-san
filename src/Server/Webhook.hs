module Server.Webhook (
  app,
  ) where

import Line.Webhook (webhookApp, Result(..))
import Network.Wai

app :: Application
app = webhookApp $ \_ -> return None -- FIXME
