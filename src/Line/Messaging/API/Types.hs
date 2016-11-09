module Line.Messaging.API.Types (
  module Line.Messaging.Common.Types,
  Location (..),
  Sticker (..),
  Text (..),
  ) where

import Data.Aeson
import Line.Messaging.Common.Types (ID(..))
import qualified Data.Text as T

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

data Sticker = Sticker { package :: ID
                       , sticker :: ID
                       }
               deriving Show

instance FromJSON Sticker where
  parseJSON (Object v) = Sticker <$> (ID <$> v .: "packageId")
                                 <*> (ID <$> v .: "stickerId")

newtype Text = Text T.Text
             deriving Show

instance FromJSON Text where
  parseJSON (Object v) = Text <$> v .: "text"
