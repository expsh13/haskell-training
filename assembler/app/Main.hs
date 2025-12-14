module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import Data.Maybe (mapMaybe)
import Parser (parseLines)
import Code (instructionToBinary)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> assembleFile inputFile
    _ -> die "Usage: assembler-exe <input.asm>"

assembleFile :: FilePath -> IO ()
assembleFile inputFile = do
  -- 入力ファイルを読み込み
  content <- readFile inputFile

  -- パース
  let instructions = parseLines content

  -- バイナリに変換（L命令はNothingなので除外される）
  let binaryLines = mapMaybe instructionToBinary instructions

  -- 出力ファイル名を生成（.asm → .hack）
  let outputFile = replaceExtension inputFile ".asm" ".hack"

  -- ファイルに書き込み
  writeFile outputFile (unlines binaryLines)

  putStrLn $ "Assembled " ++ inputFile ++ " -> " ++ outputFile
  putStrLn $ "Generated " ++ show (length binaryLines) ++ " instructions"

-- 拡張子を置き換える
replaceExtension :: FilePath -> String -> String -> FilePath
replaceExtension path oldExt newExt =
  if oldExt `isSuffixOf` path
    then take (length path - length oldExt) path ++ newExt
    else path ++ newExt
  where
    isSuffixOf suffix str =
      suffix == reverse (take (length suffix) (reverse str))
