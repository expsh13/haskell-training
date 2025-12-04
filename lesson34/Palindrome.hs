module Palindrome (isPalindrome, preprocess) where

import Data.Char (isPunctuation, isSpace, toLower)

stripWhiteSpae :: String -> String
stripWhiteSpae = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowerCase :: String -> String
toLowerCase = map toLower

preprocess :: String -> String
preprocess = stripWhiteSpae . stripPunctuation . toLowerCase

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where
    cleanText = preprocess text