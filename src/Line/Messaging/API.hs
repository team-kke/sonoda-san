module Line.Messaging.API (
  module Line.Messaging.API.Types,
  APIIO,
  runAPI,
  push,
  reply,
  getContent,
  getProfile,
  ) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.API.Types
import Line.Messaging.Types (ChannelAccessToken, ReplyToken)
import Network.Wreq (getWith, postWith, defaults, header, Options, Response, responseBody, asJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- FIXME: Add ExceptT for error handling
type APIIO a = ReaderT ChannelAccessToken IO a

runAPI :: IO ChannelAccessToken -> APIIO a -> IO a
runAPI getToken api = getToken >>= runReaderT api

getOpts :: APIIO Options
getOpts = do
  token <- encodeUtf8 <$> ask
  return $ defaults & header "Authorization" .~ ["Bearer " `B.append` token]

get :: String -> APIIO (Response BL.ByteString)
get url = do
  opts <- getOpts
  liftIO $ getWith opts url

post :: ToJSON a => String -> a -> APIIO (Response BL.ByteString)
post url body = do
  opts <- getOpts
  liftIO $ postWith opts url (toJSON body)

push :: ID -> [OutgoingMessage] -> APIIO ()
push id' ms = do
  let url = "https://api.line.me/v2/bot/message/push"
  _ <- post url $ object [ "to" .= id'
                         , "messages" .= map toJSON ms
                         ]
  return ()

reply :: ReplyToken -> [OutgoingMessage] -> APIIO ()
reply replyToken ms = do
  let url = "https://api.line.me/v2/bot/message/reply"
  _ <- post url $ object [ "replyToken" .= replyToken
                         , "messages" .= map toJSON ms
                         ]
  return ()

getContent :: ID -> APIIO BL.ByteString
getContent id' = do
  let url = concat ["https://api.line.me/v2/bot/message/"
                   , T.unpack id'
                   , "/content"
                   ]
  r <- get url
  return $ r ^. responseBody

getProfile :: ID -> APIIO Profile
getProfile id' = do
  let url = "https://api.line.me/v2/bot/profile/" ++ T.unpack id'
  r <- get url >>= asJSON
  return $ r ^. responseBody
