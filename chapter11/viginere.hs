module Viginere where

import Data.Char
import Control.Monad (forever)

type Message = String
type Keyword = String
type EncryptedMessage = String

encrypt :: Message -> Keyword -> EncryptedMessage
encrypt str kw = viginere str kwInts
  where
    kwInts = cycle $ map (modA . ord) kw
    modA = (flip mod) $ ord 'a'


viginere :: Message -> [Int] -> EncryptedMessage
viginere "" _ = ""
viginere _ [] = ""
viginere (x:xs) i@(k:ks) = if x == ' '
                           then [x] ++ viginere xs i
                           else shiftStr k [x] ++ viginere xs ks

shiftStr :: Int -> Message -> EncryptedMessage
shiftStr n = map shiftChar
  where
    alphabet = ['a'..'z']
    alphaLoop = cycle alphabet
    shiftChar c = head
        $ drop (length alphabet + n)
        $ dropWhile (/= c) alphaLoop


main :: IO ()
main = forever $ do
  putStr "Enter message: "
  msg <- getLine
  putStr "Enter keyword: "
  kw <- getLine
  putStrLn $ encrypt (map toLower msg) (map toLower kw)

