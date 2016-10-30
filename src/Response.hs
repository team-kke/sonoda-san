module Response (
  response404,
  ) where

import Network.HTTP.Types.Status
import Network.Wai

response404 :: Response
response404 =
  responseBuilder status404 [] "not found"
