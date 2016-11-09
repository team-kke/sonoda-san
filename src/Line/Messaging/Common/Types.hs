module Line.Messaging.Common.Types (
  ID (..),
  ) where

import Data.Aeson
import qualified Data.Text as T

newtype ID = ID T.Text
           deriving Show

instance FromJSON ID where
  parseJSON (Object v) = ID <$> v .: "id"
