module Line.Types (
  module Line.Messaging.Webhook.Types,
  ChannelSecret,
  ChannelAccessToken,
  ID,
  ) where

import Data.Text
import Line.Messaging.Webhook.Types

type ChannelSecret = Text
type ChannelAccessToken = Text

type ID = Text
