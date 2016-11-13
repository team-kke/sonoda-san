module Line.Messaging.API.Types (
  module Line.Messaging.Common.Types,
  Text (..),
  Image (..),
  Video (..),
  Audio (..),
  Location (..),
  Sticker (..),
  OutgoingMessage (..),
  Messageable,
  Profile (..),
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.:), (.=))
import Data.Aeson.Types (Pair)
import Line.Messaging.Common.Types (ID)
import qualified Data.Text as T

class Messageable a where
  toType :: a -> T.Text
  toObject :: a -> [Pair]

  toValue :: a -> Value
  toValue a = object $ ("type" .= toType a) : toObject a

data OutgoingMessage = TextOM Text
                     | ImageOM Image
                     | VideoOM Video
                     | AudioOM Audio
                     | LocationOM Location
                     | StickerOM Sticker
                     deriving (Eq, Show)

instance ToJSON OutgoingMessage where
  toJSON (TextOM t) = toValue t
  toJSON (ImageOM i) = toValue i
  toJSON (VideoOM v) = toValue v
  toJSON (AudioOM a) = toValue a
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

data Image = Image { imageURL :: T.Text
                   , imagePreviewURL :: T.Text
                   }
             deriving (Eq, Show)

instance Messageable Image where
  toType _ = "image"
  toObject (Image original preview) = [ "originalContentUrl" .= original
                                      , "previewImageUrl" .= preview
                                      ]

data Video = Video { videoURL :: T.Text
                   , videoPreviewURL :: T.Text
                   }
             deriving (Eq, Show)

instance Messageable Video where
  toType _ = "video"
  toObject (Video original preview) = [ "originalContentUrl" .= original
                                      , "previewImageUrl" .= preview
                                      ]

data Audio = Audio { audioURL :: T.Text
                   , audioDuration :: Integer
                   }
             deriving (Eq, Show)

instance Messageable Audio where
  toType _ = "audio"
  toObject (Audio original duration) = [ "originalContentUrl" .= original
                                       , "duration" .= duration
                                       ]

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
  parseJSON (Object v) = Sticker <$> v .: "packageId"
                                 <*> v .: "stickerId"
  parseJSON _ = fail "Sticker"

instance Messageable Sticker where
  toType _ = "sticker"
  toObject (Sticker packageId stickerId) = [ "packageId" .= packageId
                                           , "stickerId" .= stickerId
                                           ]

data Profile = Profile { userId :: ID
                       , displayName :: T.Text
                       , pictureURL :: Maybe T.Text
                       , statusMessage :: Maybe T.Text
                       }
               deriving (Eq, Show)

instance FromJSON Profile where
  parseJSON (Object v) = Profile <$> v .: "userId"
                                 <*> v .: "displayName"
                                 <*> v .: "pictureUrl"
                                 <*> v .: "statusMessage"
  parseJSON _ = fail "Profile"
