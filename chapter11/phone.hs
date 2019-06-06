module Phone where

import Data.Char
import Data.List

type Symbol = String
data DaPhone = DaPhone [(Symbol, Digit)]

convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol lol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

phone = Phone[("1", '1'), ("ABC2", '2'), ("DEF3", '3'), ("GHI4", '4'), ("JKL5", '5'),
  ("MNO6", '6'), ("PQRS7", '7'), ("TUV8", '8'), ("WXYZ9", '9'), ("+ ", '0'),
  (".,", '#'), ("^", '*') ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c = if isUpper c
                  then prependToUpper res
                  else res
  where
    cUpper = toUpper c
    res = find

maybeFindTuple :: DaPhone -> Char -> Maybe (Symbol, Digit)
maybeFindTuple p c = find (\ (a, b) -> elem cUpper a) p
  where
    cUpper = toUpper c

getTapCount :: (Symbol, Digit) -> Char -> Maybe (Digit, Presses)
getTapCount (s, d) c =
  case index of
    Just i -> 1 + i
    Nothing -> Nothing
  where
    index = elemIndex c s

prependToUpper :: [(Digit, Presses)] -> [(Digit, Presses)]
prependToUpper ls = ('*', 1) : ls
