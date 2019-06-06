module PhoneNumbers where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  areaCode <- parseNumberPlanArea
  exchange <- parseExchange
  lineNumber <- parseLineNumber
  return $ PhoneNumber areaCode exchange lineNumber


parsePhone' :: Parser PhoneNumber
parsePhone' = do
  _ <- optional $ string "1-"
  _ <- optional $ char '('
  npa <- count 3 digit
  _ <- optional $ char ')'
  _ <- optional $ oneOf " -"
  exc <- count 3 digit
  _ <- optional $ oneOf " -"
  ln <- count 4 digit
  eof
  return $ PhoneNumber (read npa) (read exc) (read ln)

getXDigits :: Int -> Parser Int
getXDigits x = read <$> count x digit


parseNumberPlanArea :: Parser Int
parseNumberPlanArea =     try parse3Digits
                      <|> try (surroundedBy parse3Digits (char '(' <|> char ')'))
                      <|> try (string "1-" *> parse3Digits)
  where parse3Digits = getXDigits 3


parseExchange :: Parser Int
parseExchange =     try parse3Digits
                <|> try (surroundedBy parse3Digits (char '-' <|> char ' '))
  where parse3Digits = getXDigits 3


parseLineNumber :: Parser Int
parseLineNumber = try (getXDigits 4) <|> try (char '-' *> getXDigits 4)
