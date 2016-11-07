module Line.Types (
  module Line.Messaging.Webhook.Types,
  ChannelSecret,
  ) where

import Data.Text
import Line.Messaging.Webhook.Types

type ChannelSecret = Text
