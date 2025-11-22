import Control.Monad (foldM)
import Text.Read (readMaybe)

-- 入力: 5 2 3 + *

-- Step 1: 5
--   スタック: [5]

-- Step 2: 2
--   スタック: [2, 5]

-- Step 3: 3
--   スタック: [3, 2, 5]

-- Step 4: +
--   3 + 2 = 5
--   スタック: [5, 5]

-- Step 5: *
--   5 * 5 = 25
--   スタック: [25]

-- 結果: 25

type Stack = [Double]

data Ope = Add | Sub | Multi | Div deriving (Show)

parseOpe :: String -> Maybe Ope
parseOpe "+" = Just Add
parseOpe "-" = Just Sub
parseOpe "*" = Just Multi
parseOpe "/" = Just Div
parseOpe _ = Nothing

-- splitArr :: [String] -> [Int] -> [Ope] -> ([Int], [Ope])
-- -- 文字列配列がなければ結果出力
-- splitArr [] numArray opeArray = (numArray, opeArray)
-- --
-- splitArr (input : rest) numArray opeArray =
--   case readMaybe input :: Maybe Int of
--     Just n -> splitArr rest (n : numArray) opeArray
--     -- 文字列がオペランドかどうか判定。
--     Nothing -> case parseOpe input of
--       Just ope -> splitArr rest numArray (opeArray ++ [ope])
--       Nothing -> ([], [])

-- calc :: Ope -> Int -> Int -> Int
-- calc Add a b = a + b
-- calc Sub a b = a - b
-- calc Multi a b = a * b

-- calcArray :: [Int] -> [Ope] -> Int
-- -- オペランドが空
-- calcArray num [] = head num
-- -- numArray
-- calcArray [] opeArray = 0
-- calcArray [num] opeArray = num
-- -- 数値配列が二つ以上、opeが一つ以上であれば計算して、再帰
-- calcArray (fstNum : sndNum : numRest) (ope : opeRest) = calcArray (calc ope fstNum sndNum : numRest) opeRest

applyOpe :: Ope -> Stack -> Maybe Stack
applyOpe Add (y : x : rest) = Just ((x + y) : rest)
applyOpe Sub (y : x : rest) = Just ((x - y) : rest)
applyOpe Multi (y : x : rest) = Just ((x * y) : rest)
applyOpe Div (y : x : rest) = Just ((x / y) : rest)
applyOpe _ _ = Nothing -- スタックに2つない場合

-- トークン一つを処理
processToken :: Stack -> String -> Maybe Stack
processToken stack token =
  case readMaybe token :: Maybe Double of
    Just n -> Just (n : stack)
    Nothing -> case parseOpe token of
      Just ope -> applyOpe ope stack
      Nothing -> Nothing

-- トークン列を処理
processTokens :: [String] -> Maybe Stack
processTokens = foldM processToken []

inputLoop :: IO ()
inputLoop = do
  putStr "入力: "
  input <- getLine
  if input == "quit"
    then putStrLn "Goodbye!"
    else do
      let tokens = words input
      case processTokens tokens of
        Nothing -> do
          putStrLn "Error: Invalid expression"
          inputLoop
        Just [] -> do
          putStrLn "Error: Empty stack"
          inputLoop
        Just (result : _) -> do
          print result
          inputLoop

-- let arr = words input
-- let result = splitArr arr [] []
-- case result of
--   ([], []) -> do
--     putStrLn "適切な値を入力してください"
--     inputLoop
--   (numArray, opeArray) -> do
--     let result = calcArray numArray opeArray
--     print result

main :: IO ()
main = do
  putStrLn "RPN Calculator (type 'quit' to exit)"
  inputLoop
