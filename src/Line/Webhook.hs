module Line.Webhook (
  module Line.Webhook.Event,
  module Line.Webhook.Types,
  webhookApp,
  defaultOnFailure,
  ) where

import Data.Aeson (decode')
import Data.ByteString.Builder (string8)
import Line.Webhook.Event
import Line.Webhook.Types
import Line.Webhook.Validation (validateSignature)
import Network.HTTP.Types.Status
import Network.Wai

webhookApp :: ChannelSecret
           -> ([Event] -> IO WebhookResult)
           -> (WebhookFailure -> Application)
           -> Application
webhookApp secret handler fail req f
  | not (validateSignature secret req) =
      fail SignatureVerificationFailed req f
  | otherwise = do
      body <- lazyRequestBody req
      case decode' body of
        Nothing -> fail MessageDecodeFailed req f
        Just events -> do
          result <- handler events
          case result of
            Ok -> f $ responseBuilder status200 [] ""
            WaiResponse res -> f res
            WaiApp app -> app req f

defaultOnFailure :: WebhookFailure -> Application
defaultOnFailure failure _ f = f .
  responseBuilder status400 [] . string8 . show $ failure
