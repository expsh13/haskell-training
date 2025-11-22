-- # Title
-- ## Subtitle
-- ### Heading 3
-- - Item 1
-- - Item 2
-- This is `code` here.

-- <h1>Title</h1>
-- <h2>Subtitle</h2>
-- <h3>Heading 3</h3>
-- <ul>
--   <li>Item 1</li>
--   <li>Item 2</li>
-- </ul>
-- This is <code>code</code> here.

parseStr :: String -> String
parseStr str =
  case words str of
    "#" : rest -> "<h1>" ++ unwords rest ++ "</h1>"
    "##" : rest -> "<h2>" ++ unwords rest ++ "</h2>"
    "###" : rest -> "<h3>" ++ unwords rest ++ "</h3>"
    _ -> str

parseList :: String -> String
parseList item = "  <li>" ++ drop 2 item ++ "</li>"

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
  False -> parseStr x : parseMarkdown xs

main :: IO ()
main = do
  putStrLn "Enter markdown (Ctrl+D to finish):"
  contents <- getContents
  -- 行ごとに配列へ
  let contentsArrayByLine = lines contents
  let result = unlines (parseMarkdown contentsArrayByLine)
  putStrLn "\nHTML:"
  putStrLn result
