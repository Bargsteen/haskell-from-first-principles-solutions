module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators


stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParseS :: Parser String -> IO ()
testParseS p =
  print $ parseString p mempty "123"

-- Exercise: Parsing Practice
-- 1. eof
one'' = one >> eof >> stop
oneTwo'' = oneTwo >> eof >> stop


-- 2. String Parsers
oneS = string "1"

oneTwoS = string "12"

oneTwoThreeS = string "123"

allThree :: Parser String
allThree = choice [oneTwoThreeS, oneTwoS, oneS]

-- 3. string' using char
string' :: String -> Parser String
string' str = go mempty str
  where
    go :: Parser String -> String -> Parser String
    go res [] = res
    go res (x:xs) = return <$> char x >>= (\r -> (r ++) <$> go res xs)

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "one'':"
  testParse one''
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwo'':"
  testParse oneTwo''
  pNL "oneS:"
  testParseS oneS
  pNL "oneTwoS:"
  testParseS oneTwoS
  pNL "oneTwoThreeS:"
  testParseS oneTwoThreeS
