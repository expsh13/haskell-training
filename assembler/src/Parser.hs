module Parser
  ( Instruction(..)
  , cleanLine
  , parseInstruction
  , parseLines
  ) where

import Text.Read (readMaybe)

data Instruction
  = AInstruction Int
  | CInstruction
      { dest :: Maybe String,
        comp :: String,
        jump :: Maybe String
      }
  | LInstruction String
  deriving (Show, Eq)

-- 空白・コメントの削除
cleanLine :: String -> String
cleanLine = cleanBlank . cleanComment

-- 空白の削除
cleanBlank :: String -> String
cleanBlank = concat . words

-- コメントの削除
cleanComment :: String -> String
cleanComment [] = []
cleanComment ('/' : '/' : _) = []
cleanComment (x : xs) = x : cleanComment xs

-- 命令のパース
parseInstruction :: String -> Maybe Instruction
parseInstruction "" = Nothing
parseInstruction ('@' : xs) = AInstruction <$> readMaybe xs
parseInstruction ('(' : xs) = Just $ LInstruction xs'
  where
    xs' = takeWhile (/= ')') xs
parseInstruction str = case break (== '=') str of
  (d, '=' : rest) -> Just $ CInstruction (Just d) comp' jump'
    where
      (compPart, jumpPart) = break (== ';') rest
      comp' = compPart
      jump' = if null jumpPart then Nothing else Just (tail jumpPart)
  _ -> case break (== ';') str of
    (compPart, ';' : jumpPart) -> Just $ CInstruction Nothing comp' (Just jumpPart)
      where
        comp' = compPart
    _ -> Just $ CInstruction Nothing str Nothing

-- 複数行のパース
parseLines :: String -> [Instruction]
parseLines = foldr collectInstructions [] . lines
  where
    collectInstructions line acc =
      case parseInstruction (cleanLine line) of
        Just instr -> instr : acc
        Nothing -> acc