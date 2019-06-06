module SemVer where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators
import Data.Functor.Identity
-- Relevant to precedence/ordering
-- cannot sort numbers like strings

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type MetaData = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release MetaData
  deriving (Eq, Show, Ord)

parseSemVer :: Parser SemVer
parseSemVer = do
      (major, minor, patch) <- parseVersion
      release' <- try parseRelease <|> return []
      metaData <- try parseMetaData <|> return []
      _ <- eof
      return $ SemVer major minor patch release' metaData


parseVersion :: Parser (Major, Minor, Patch)
parseVersion = (,,) <$> (int <* dot) <*> (int <* dot) <*> int

parseRelease :: Parser Release
parseRelease = parseNOSDottedList '-'

parseMetaData :: Parser MetaData
parseMetaData = parseNOSDottedList '+'

parseNOSDottedList :: Char -> Parser [NumberOrString]
parseNOSDottedList c = char c *>
  some (try (parseNumOrString <* dot) <|> parseNumOrString)


int :: Parser Integer
int = read <$> some (char '0' <|> char '1' <|> char '2'
                     <|> char '3' <|> char '4' <|> char '5'
                     <|> char '6' <|> char '7' <|> char '8'
                     <|> char '9')

parseNumOrString :: Parser NumberOrString
parseNumOrString = NOSS <$> some letter <|> NOSI <$> int


ps = parseString parseSemVer mempty

precedenceIsCorrect :: Bool
precedenceIsCorrect = undefined --isSharplySorted versions
  where
  versions = map ps ["1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha-beta", "1.0.0-beta",
              "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1", "1.0.0"]


isSharplySorted :: (Ord a) => [a] -> Bool
isSharplySorted xs = all (uncurry (<)) . zip xs $ tail xs
