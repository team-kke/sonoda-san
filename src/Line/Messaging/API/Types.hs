module Line.Messaging.API.Types (
  module Line.Messaging.Common.Types,
  Location (..),
  Sticker (..),
  Text (..),
  OutgoingMessage,
  Messageable (toMessage)
  ) where

import Data.Aeson (FromJSON(..), Value(..), object, (.:), (.=))
import Data.Aeson.Types (Pair)
import Line.Messaging.Common.Types (ID(..))
import qualified Data.Text as T

type OutgoingMessage = Value

class Messageable a where
  toType :: a -> T.Text
  toObject :: a -> [Pair]

  toMessage :: a -> OutgoingMessage
  toMessage a = object $ ("type" .= toType a) : toObject a

newtype Text = Text T.Text
             deriving Show

instance FromJSON Text where
  parseJSON (Object v) = Text <$> v .: "text"

instance Messageable Text where
  toType _ = "text"
  toObject (Text text) = [ "text" .= text ]

-- FIXME: Implement Messageable instances for https://devdocs.line.me/en/#send-message-object

data Location = Location { title :: T.Text
                         , address :: T.Text
                         , latitude :: Double
                         , longitude :: Double
                         }
                deriving Show

instance FromJSON Location where
  parseJSON (Object v) = Location <$> v .: "title"
                                  <*> v .: "address"
                                  <*> v .: "latitude"
                                  <*> v .: "longitude"

instance Messageable Location where
  toType _ = "location"
  toObject location = [ "title" .= title location
                      , "address" .= address location
                      , "latitude" .= latitude location
                      , "logiture" .= longitude location
                      ]

data Sticker = Sticker { package :: ID
                       , sticker :: ID
                       }
               deriving Show

instance FromJSON Sticker where
  parseJSON (Object v) = Sticker <$> (ID <$> v .: "packageId")
                                 <*> (ID <$> v .: "stickerId")

instance Messageable Sticker where
  toType _ = "sticker"
  toObject (Sticker (ID packageId) (ID stickerId)) = [ "packageId" .= packageId
                                                     , "stickerId" .= stickerId
                                                     ]
