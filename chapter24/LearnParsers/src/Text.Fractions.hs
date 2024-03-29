{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"


parseFractionBad :: (Monad m, TokenParsing m)
              => m Rational
parseFractionBad = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)


parseFraction :: (Monad m, TokenParsing m)
              => m Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)


-- Exercise: integer then eof
intEof :: Parser Integer
--intEof = integer >>= (\int -> eof >>= (\_ -> return int))
intEof = do
  int <- integer
  _ <- eof
  return int

-- Exercise: Try Try
type FracOrDecimal = Either Integer Rational


fracOrDecimal :: Parser FracOrDecimal
fracOrDecimal =
  (Left <$> try intEof) <|> (Right <$> try parseFraction)

main :: IO ()
main = do
  -- parseOnly is Attoparsec
  let attoP = parseOnly parseFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad

  -- parseString is Trifecta
  let p f =
        parseString f mempty
  print $ p parseFraction badFraction
  print $ p parseFraction shouldWork
  print $ p parseFraction shouldAlsoWork
  print $ p parseFraction alsoBad
