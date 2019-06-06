-- # LANGUAGE OverloadedStrings #
{-# LANGUAGE QuasiQuotes #-}

module LogParser where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators (someTill, skipManyTill)
import Data.Time.LocalTime
import Text.RawString.QQ
import Text.Trifecta


day1 :: String
day1 = [r|
-- whee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

day2 :: String
day2 = [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]


type Activity = String
data Entry = Entry TimeOfDay Activity deriving (Eq, Show)
data Log = Log Date [Entry] deriving (Eq, Show)
type Year = Int
type Month = Int
type Day = Int
data Date = Date Year Month Day deriving (Eq, Show)



--instance Show Log where
  --show (Log date entries) = "# " ++ show date

stop :: Parser a
stop = unexpected "stop"


parseLog :: Parser Log
parseLog = do
  _ <- optional commentsOrNewlines
  date <- parseDate
  _ <- optional commentsOrNewlines
  logEntries <- sepBy1 parseEntry commentsOrNewlines
  return $ Log date logEntries


parseComment :: Parser String
parseComment = (string " --" <|> string "--") *> manyTill anyChar newline <?> "comment"

parseTime :: Parser TimeOfDay
parseTime = TimeOfDay <$> (read <$> count 2 digit) <*> (colon *> (read <$> count 2 digit))
  <*> pure 0 <?> "Tried to parse time"
  -- do
  -- hours <- read <$> count 2 digit
  -- _ <- char ':'
  -- minutes <- read <$> count 2 digit
  -- return $ TimeOfDay hours minutes 0

commentsOrNewlines = many (parseComment <|> pure <$> newline) <?> "comments or newlines"

parseDate :: Parser Date
parseDate = do
  _ <- string "# "
  year <- read <$> count 4 digit <* char '-'
  month <- read <$> count 2 digit <* char '-'
  day <- read <$> count 2 digit
  return $ Date year month day

parseEntry :: Parser Entry
parseEntry = Entry <$> parseTime <*> (skipSome space *>
                                      someTill anyChar (pure <$> newline <|> string "--"))
             <?> "entry"


ps p s = parseString p mempty s
