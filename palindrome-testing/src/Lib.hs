module Lib
  ( isPalindrome,
    preprocess,
  )
where

import Data.Char (isPunctuation)

preprocess :: String -> String
preprocess = filter (not . isPunctuation)

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where
    cleanText = preprocess text
