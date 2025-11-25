import Data.List.Split (splitOn)
import Text.Read (readMaybe)

data List = OrderList | UnOrderList

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
parseList item = "  <li>" ++ parseInline planeText ++ "</li>"
  where
    planeText = case words item of
      [] -> ""
      (_ : rest) -> unwords rest

parseListGroup :: String -> String -> [String] -> [String]
parseListGroup openTag closeTag items = openTag : parsedList ++ [closeTag]
  where
    parsedList = map parseList items

-- unOrderListSpan :: [String] -> ([String], [String])
-- unOrderListSpan = span isUnOrderList

isUnOrderList :: String -> Bool
isUnOrderList item = case words item of
  "-" : _ -> True
  _ -> False

isOrderList :: String -> Bool
isOrderList item =
  case words item of
    [] -> False
    (firstWord : _) ->
      case readMaybe (init firstWord) :: Maybe Int of
        Nothing -> False
        Just _ -> not (null firstWord) && last firstWord == '.'

isList :: String -> Maybe List
isList lineStr
  | isUnOrderList lineStr = Just UnOrderList
  | isOrderList lineStr = Just OrderList
  | otherwise = Nothing

-- 全行の配列を受け取る
parseMarkdown :: [String] -> [String]
parseMarkdown [] = []
parseMarkdown (x : xs) = case isList x of
  -- リストの時
  Just UnOrderList -> parseListGroup "<ul>" "</ul>" list ++ parseMarkdown noList
    where
      (list, noList) = span isUnOrderList (x : xs)
  Just OrderList -> parseListGroup "<ol>" "</ol>" list ++ parseMarkdown noList
    where
      (list, noList) = span isOrderList (x : xs)
  Nothing -> parseLine x : parseMarkdown xs

main :: IO ()
main = do
  putStrLn "Enter markdown (Ctrl+D to finish):"
  contents <- getContents
  -- 行ごとに配列へ
  let contentsArrayByLine = lines contents
  let result = unlines (parseMarkdown contentsArrayByLine)
  putStrLn "\nHTML:"
  putStrLn result
