module Response (
  response200,
  response404,
  ) where

import Data.ByteString.Builder (Builder)
import Network.HTTP.Types.Status
import Network.Wai

response200 :: Builder -> Response
response200 =
  responseBuilder status200 []

response404 :: Response
response404 =
  responseBuilder status404 [] "not found"
