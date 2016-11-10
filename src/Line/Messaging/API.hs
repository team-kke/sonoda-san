module Line.Messaging.API (
  module Line.Messaging.API.Types,
  APIIO,
  runAPI,
  push,
  reply,
  ) where

import Control.Lens ((&), (.~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.API.Types
import Line.Messaging.Types (ChannelAccessToken, ReplyToken)
import Network.Wreq
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
  liftIO $ postWith opts (url apiPath) (toJSON body) >> return ()

push :: ID -> [OutgoingMessage] -> APIIO ()
push (ID i) ms = request "push" $ object [ "to" .= i
                                         , "messages" .= map toJSON ms
                                         ]

reply :: ReplyToken -> [OutgoingMessage] -> APIIO ()
reply replyToken ms = request "reply" $ object [ "replyToken" .= replyToken
                                               , "messages" .= map toJSON ms
                                               ]
