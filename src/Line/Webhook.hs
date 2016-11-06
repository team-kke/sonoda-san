module Line.Webhook (
  module Line.Webhook.Event,
  module Line.Webhook.Types,
  webhook,
  webhookApp,
  defaultOnFailure,
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Data.Aeson (decode')
import Data.ByteString.Builder (string8)
import Line.Webhook.Event
import Line.Webhook.Types
import Line.Webhook.Validation (validateSignature)
import Network.HTTP.Types.Status
import Network.Wai

webhook :: ChannelSecret
        -> Request
        -> ExceptT WebhookFailure IO [Event]
webhook secret req = do
  body <- liftIO $ lazyRequestBody req
  if not $ validateSignature secret req body
  then throwE SignatureVerificationFailed
  else do
    case decode' body of
      Nothing -> throwE MessageDecodeFailed
      Just (Body events) -> return events

waiResponse :: WebhookResult -> Application
waiResponse result req f = case result of
  Ok              -> f $ responseBuilder status200 [] ""
  WaiResponse res -> f res
  WaiApp app      -> app req f

webhookApp :: ChannelSecret
           -> ([Event] -> IO WebhookResult)
           -> (WebhookFailure -> Application)
           -> Application
webhookApp secret handler fail req f = do
  result <- runExceptT $ webhook secret req
  case result of
    Right events -> handler events >>= waiResponse <*> pure req <*> pure f
    Left exception -> fail exception req f

defaultOnFailure :: WebhookFailure -> Application
defaultOnFailure failure _ f = f .
  responseBuilder status400 [] . string8 . show $ failure
