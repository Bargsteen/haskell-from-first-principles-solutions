module Palindrome where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line = (concat . words . map toLower) line1
  case (line == reverse line) of
    True -> putStrLn "It's a palindrome!"
    False -> exitSuccess
