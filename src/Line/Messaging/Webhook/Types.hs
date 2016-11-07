module Line.Messaging.Webhook.Types (
  WebhookResult (..),
  WebhookFailure (..),
  ReplyToken,
  ) where

import Data.Text
import Network.Wai

data WebhookResult = Ok
                   | WaiResponse Response
                   | WaiApp Application

data WebhookFailure = SignatureVerificationFailed
                    | MessageDecodeFailed
                    deriving Show

type ReplyToken = Text
