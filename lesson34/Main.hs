module Main where

import Palindrome qualified

main :: IO ()
main = do
  putStrLn "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  let response =
        if Palindrome.isPalindrome text
          then "it is!"
          else "it's not!"
  print response