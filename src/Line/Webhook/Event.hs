module Line.Webhook.Event (
  Event (..),
  ) where

import Data.Aeson

-- The Event data type and instances for proper type classes (e.g. FromJson)
-- should be implemented here.
-- For the Event spec, please refer to the official doc.
--
-- https://devdocs.line.me/en/#webhook-event-object

data Event = Undefined

instance FromJSON Event where
  parseJSON (Object v) = undefined
