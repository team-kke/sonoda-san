module Line.Messaging.API.Types (
  module Line.Messaging.Common.Types,
  Location (..),
  Sticker (..),
  Text (..),
  OutgoingMessage (..),
  Messageable,
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.:), (.=))
import Data.Aeson.Types (Pair)
import Line.Messaging.Common.Types (ID(..))
import qualified Data.Text as T

class Messageable a where
  toType :: a -> T.Text
  toObject :: a -> [Pair]

  toValue :: a -> Value
  toValue a = object $ ("type" .= toType a) : toObject a

data OutgoingMessage = TextOM Text
                     | LocationOM Location
                     | StickerOM Sticker
                     deriving (Eq, Show)

instance ToJSON OutgoingMessage where
  toJSON (TextOM t) = toValue t
  toJSON (LocationOM l) = toValue l
  toJSON (StickerOM s) = toValue s

newtype Text = Text T.Text
             deriving (Eq, Ord, Show)

instance FromJSON Text where
  parseJSON (Object v) = Text <$> v .: "text"
  parseJSON (String text) = return $ Text text
  parseJSON _ = fail "Text"

instance Messageable Text where
  toType _ = "text"
  toObject (Text text) = [ "text" .= text ]

-- FIXME: Implement Messageable instances for https://devdocs.line.me/en/#send-message-object

data Location = Location { title :: T.Text
                         , address :: T.Text
                         , latitude :: Double
                         , longitude :: Double
                         }
                deriving (Eq, Show)

instance FromJSON Location where
  parseJSON (Object v) = Location <$> v .: "title"
                                  <*> v .: "address"
                                  <*> v .: "latitude"
                                  <*> v .: "longitude"
  parseJSON _ = fail "Location"

instance Messageable Location where
  toType _ = "location"
  toObject location = [ "title" .= title location
                      , "address" .= address location
                      , "latitude" .= latitude location
                      , "longitude" .= longitude location
                      ]

data Sticker = Sticker { package :: ID
                       , sticker :: ID
                       }
               deriving (Eq, Show)

instance FromJSON Sticker where
  parseJSON (Object v) = Sticker <$> (ID <$> v .: "packageId")
                                 <*> (ID <$> v .: "stickerId")
  parseJSON _ = fail "Sticker"

instance Messageable Sticker where
  toType _ = "sticker"
  toObject (Sticker (ID packageId) (ID stickerId)) = [ "packageId" .= packageId
                                                     , "stickerId" .= stickerId
                                                     ]
