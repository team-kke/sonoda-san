module Line.Webhook.Types (
  ChannelSecret,
  WebhookResult (..),
  WebhookFailure (..),
  ) where

import Data.Text
import Network.Wai

data WebhookResult = Ok
                   | WaiResponse Response
                   | WaiApp Application

data WebhookFailure = SignatureVerificationFailed
                    | MessageDecodeFailed
                    deriving Show

type ChannelSecret = Text
