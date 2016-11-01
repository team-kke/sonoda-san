module Line.Webhook.Validation (
  validateSignature,
  ) where

import Network.Wai

validateSignature :: Request -> Bool
validateSignature _ = undefined
