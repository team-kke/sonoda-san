module Line.Webhook.Validation (
  validateSignature,
  ) where

import Line.Webhook.Types
import Network.Wai

validateSignature :: ChannelSecret -> Request -> Bool
validateSignature _ _ = undefined
