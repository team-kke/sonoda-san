{-# LANGUAGE OverloadedStrings #-}

module Line.Webhook.Validation (
  validateSignature,
  ) where

import Crypto.Hash.SHA256 (hmaclazy)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Line.Webhook.Types
import Network.HTTP.Types.Header
import Network.Wai
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as L

getSignature :: Request -> Maybe ByteString
getSignature req = lookup "X-Line-Signature" headers
  where
    headers = requestHeaders req

validateSignature :: ChannelSecret -> Request -> L.ByteString -> Bool
validateSignature secret req body = case getSignature req of
  Nothing -> False
  Just signature -> hash == signature
  where
    hash = Base64.encode $ hmaclazy (encodeUtf8 secret) body
