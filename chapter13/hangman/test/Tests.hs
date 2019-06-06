module Test where

import           Main
import           Test.Hspec
import           Test.QuickCheck


--puzzleHasJustChar :: Puzzle ->


main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "inserts letter if correct" $ do
      fillInCharacter (freshPuzzle "word") 'w'


{-
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar

    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar
      -}
