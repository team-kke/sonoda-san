module Line.Webhook (
  module Line.Webhook.Event,
  module Line.Webhook.Types,
  webhookApp,
  defaultOnFailure
  ) where

import Data.Aeson (decode')
import Line.Webhook.Event
import Line.Webhook.Types
import Line.Webhook.Validation (validateSignature)
import Network.HTTP.Types.Status
import Network.Wai

webhookApp :: ([Event] -> IO WebhookResult)
           -> (WebhookFailure -> Application)
           -> Application
webhookApp handler fail req f
  | validateSignature req = fail SignatureVerificationFailed req f
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
defaultOnFailure = undefined
