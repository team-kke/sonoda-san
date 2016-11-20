module Calculator (
  calc,
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (decimal, char, skipSpace)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

calc :: T.Text -> T.Text
calc x = T.pack $ case calcInternal . TE.encodeUtf8 $ x of
  Right result -> show result
  Left err -> err

calcInternal :: B.ByteString -> Either String Integer
calcInternal = parseOnly expr

expr :: Parser Integer
expr = addExpr

addExpr :: Parser Integer
addExpr = do
  a <- mulExpr
  skipSpace
  b <- plus <|> return 0
  return $ a + b
    where plus = do
            _ <- char '+'
            skipSpace
            expr

mulExpr :: Parser Integer
mulExpr = do
  a <- decimal
  skipSpace
  b <- mul <|> return 1
  return $ a * b
    where mul = do
            _ <- char '*'
            skipSpace
            decimal
