module Line.Messaging.Webhook.Types (
  WebhookResult (..),
  WebhookFailure (..),
  ) where

import Network.Wai

data WebhookResult = Ok
                   | WaiResponse Response
                   | WaiApp Application

data WebhookFailure = SignatureVerificationFailed
                    | MessageDecodeFailed
                    deriving Show
