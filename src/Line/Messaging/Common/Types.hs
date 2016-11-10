module Line.Messaging.Common.Types (
  ID (..),
  ChannelSecret,
  ChannelAccessToken,
  ) where

import Data.Aeson
import qualified Data.Text as T

newtype ID = ID { toText :: T.Text }
           deriving (Eq, Ord, Show)

instance FromJSON ID where
  parseJSON (Object v) = ID <$> v .: "id"
  parseJSON (String text) = return $ ID text
  parseJSON _ = fail "ID"

type ChannelSecret = T.Text
type ChannelAccessToken = T.Text
