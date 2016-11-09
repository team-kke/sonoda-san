module Line.Messaging.Types (
  module Line.Messaging.Webhook.Types,
  ChannelSecret,
  ChannelAccessToken,
  ) where

import Data.Text
import Line.Messaging.Webhook.Types

type ChannelSecret = Text
type ChannelAccessToken = Text
