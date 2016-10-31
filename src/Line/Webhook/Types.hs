module Line.Webhook.Types (
  Event (..),
  WebhookResult (..),
  WebhookFailure (..),
  ) where

import Network.Wai

-- FIXME
data Event

data WebhookResult = Ok
                | WaiResponse Response
                | WaiApp Application

data WebhookFailure = SignatureVerificationFailed deriving Show
