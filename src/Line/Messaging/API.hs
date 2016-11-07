module Line.Messaging.API
  ( reply
  ) where

import Line.Types
import qualified Data.Text as T

-- FIXME: send message objects instead of text
-- FIXME: return response object
reply :: ChannelAccessToken -> ReplyToken -> T.Text -> IO ()
reply accessToken replyToken text = return undefined
