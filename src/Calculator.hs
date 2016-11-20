module Calculator (
  calc,
  ) where

import qualified Data.Text as T

calc :: T.Text -> T.Text
calc = T.pack . show . calcInternal . T.unpack

calcInternal :: String -> Integer
calcInternal = undefined
