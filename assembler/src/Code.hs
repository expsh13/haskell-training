module Code
  ( destToBits
  , compToBits
  , jumpToBits
  , instructionToBinary
  ) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Parser (Instruction(..))

destToBits :: Maybe String -> String
destToBits dest = case dest of
  Nothing -> "000"
  Just d ->
    [ if 'A' `elem` d then '1' else '0',
      if 'D' `elem` d then '1' else '0',
      if 'M' `elem` d then '1' else '0'
    ]

compToBits :: String -> String
compToBits comp = case comp of
  "0" -> "0101010"
  "1" -> "0111111"
  "-1" -> "0111010"
  "D" -> "0001100"
  "A" -> "0110000"
  "!D" -> "0001101"
  "!A" -> "0110001"
  "-D" -> "0001111"
  "-A" -> "0110011"
  "D+1" -> "0011111"
  "A+1" -> "0110111"
  "D-1" -> "0001110"
  "A-1" -> "0110010"
  "D+A" -> "0000010"
  "D-A" -> "0010011"
  "A-D" -> "0000111"
  "D&A" -> "0000000"
  "D|A" -> "0010101"
  "M" -> "1110000"
  "!M" -> "1110001"
  "-M" -> "1110011"
  "M+1" -> "1110111"
  "M-1" -> "1110010"
  "D+M" -> "1000010"
  "D-M" -> "1010011"
  "M-D" -> "1000111"
  "D&M" -> "1000000"
  "D|M" -> "1010101"
  _ -> error $ "Invalid comp field: " ++ comp

jumpToBits :: Maybe String -> String
jumpToBits jump = case jump of
  Nothing -> "000"
  Just j -> case j of
    "JGT" -> "001"
    "JEQ" -> "010"
    "JGE" -> "011"
    "JLT" -> "100"
    "JNE" -> "101"
    "JLE" -> "110"
    "JMP" -> "111"
    _ -> error $ "Invalid jump field: " ++ j

-- 命令をバイナリコードに変換
instructionToBinary :: Instruction -> Maybe String
instructionToBinary (AInstruction n) =
  Just $ "0" ++ padLeft 15 '0' (intToBinary n)
instructionToBinary (CInstruction dest comp jump) =
  Just $ "111" ++ compToBits comp ++ destToBits dest ++ jumpToBits jump
instructionToBinary (LInstruction _) =
  Nothing  -- L命令は機械語に変換されない

-- 整数を2進数文字列に変換
intToBinary :: Int -> String
intToBinary n = showIntAtBase 2 intToDigit n ""

-- 文字列を指定された長さまで左側を埋める
padLeft :: Int -> Char -> String -> String
padLeft n c s = replicate (n - length s) c ++ s