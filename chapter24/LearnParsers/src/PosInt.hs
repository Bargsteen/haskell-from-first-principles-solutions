module PosInt where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators

parseDigit :: Parser Char
parseDigit = foldr (<|>) (char '0') ([char] <*> ['1'..'9'])

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit


base10Integer' :: Parser Integer
base10Integer' =
  try (char '-' *> (negate <$> base10Integer)) <|> try base10Integer
