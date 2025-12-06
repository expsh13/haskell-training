import Data.Char (isPunctuation)
import Lib (preprocess)
import Test.QuickCheck

prop_punctuationInvariant :: [Char] -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = filter (not . isPunctuation) text

main :: IO ()
main = do
  quickCheck prop_punctuationInvariant
  putStrLn "done!"
