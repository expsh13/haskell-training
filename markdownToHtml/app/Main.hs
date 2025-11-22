import Data.List.Split (splitOn)

processParts :: String -> String -> [(Integer, String)] -> String
processParts openTag closeTag = foldl addTag ""
  where
    addTag acc (index, str) =
      if odd index
        then acc ++ str
        else acc ++ openTag ++ str ++ closeTag

replaceWith :: String -> String -> String -> String -> String
replaceWith delimiter openTag closeTag str =
  if odd arrayLength
    then processParts openTag closeTag tuple
    else str
  where
    splitArray = splitOn delimiter str
    arrayLength = length splitArray
    tuple = zip [1 ..] splitArray

replaceCode = replaceWith "`" "<code>" "</code>"

replaceBold = replaceWith "**" "<strong>" "</strong>"

replaceItalic = replaceWith "*" "<em>" "</em>"

replaceStrike = replaceWith "~~" "<s>" "</s>"

parseInline :: String -> String
parseInline str = replaceStrike $ replaceItalic $ replaceBold $ replaceCode str

parseLine :: String -> String
parseLine str =
  case words str of
    "#" : rest -> "<h1>" ++ parseInline (unwords rest) ++ "</h1>"
    "##" : rest -> "<h2>" ++ parseInline (unwords rest) ++ "</h2>"
    "###" : rest -> "<h3>" ++ parseInline (unwords rest) ++ "</h3>"
    "---" : rest -> (if isHr then "<hr>" else parseInline str)
      where
        isHr = null rest
    _ -> parseInline str

parseList :: String -> String
parseList item = "  <li>" ++ parseInline (drop 2 item) ++ "</li>"

parseListGroup :: [String] -> [String]
parseListGroup items = open : parsedList ++ [close]
  where
    open = "<ul>"
    close = "</ul>"
    parsedList = map parseList items

isListItem :: String -> Bool
isListItem item = case words item of
  "-" : _ -> True
  _ -> False

listSpan :: [String] -> ([String], [String])
listSpan = span isListItem

-- 全行の配列を受け取る
parseMarkdown :: [String] -> [String]
parseMarkdown [] = []
parseMarkdown (x : xs) = case isListItem x of
  -- リストの時
  True -> parseListGroup list ++ parseMarkdown noList
    where
      (list, noList) = listSpan (x : xs)
  False -> parseLine x : parseMarkdown xs

main :: IO ()
main = do
  putStrLn "Enter markdown (Ctrl+D to finish):"
  contents <- getContents
  -- 行ごとに配列へ
  let contentsArrayByLine = lines contents
  let result = unlines (parseMarkdown contentsArrayByLine)
  putStrLn "\nHTML:"
  putStrLn result
