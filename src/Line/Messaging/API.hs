module Line.Messaging.API (
  runAPI,
  reply,
  ) where

import Control.Lens ((&), (.~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Text.Encoding (encodeUtf8)
import Line.Types
import Network.Wreq
import Network.Wreq.Types (Postable)
import qualified Data.Text as T
import qualified Data.ByteString as B

url :: String -> String
url = (++) "https://api.line.me/v2/bot/message/"

type APIIO a = ReaderT ChannelAccessToken IO a

runAPI :: IO ChannelAccessToken -> APIIO a -> IO a
runAPI getToken api = getToken >>= runReaderT api

-- FIXME: return response object
request :: ToJSON a => String -> a -> APIIO ()
request apiPath body = do
  token <- encodeUtf8 <$> ask
  let opts = defaults & header "Authorization" .~ ["Bearer " `B.append` token]
  liftIO $ do
    postWith opts (url apiPath) (toJSON body)
    return ()

-- FIXME: separate into Types module
data Message = Text T.Text

instance ToJSON Message where
  toJSON (Text t) = object [ "type" .= ("text" :: T.Text)
                           , "text" .= t
                           ]

-- FIXME: send message objects instead of text
reply :: ReplyToken -> T.Text -> APIIO ()
reply replyToken text = request "reply" $ object [ "replyToken" .= replyToken
                                                 , "messages" .= [ Text text ]
                                                 ]
