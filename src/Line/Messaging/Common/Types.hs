module Line.Messaging.Common.Types (
  ID,
  ChannelSecret,
  ChannelAccessToken,
  ) where

import qualified Data.Text as T

type ID = T.Text

type ChannelSecret = T.Text
type ChannelAccessToken = T.Text
