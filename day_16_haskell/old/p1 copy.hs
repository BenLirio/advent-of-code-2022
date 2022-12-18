data Node = Node { name :: String, rate :: Int, children :: [String] }

main :: IO ()
main = do
  s <- readFile "input_small.txt"
  let nodes = parseInput s
  printNodes nodes

parseNode :: String -> Node
parseNode s = do
  Node name rate children
  where
    name = takeWhile (/= ' ') $ drop 1 $ dropWhile (/= ' ') s
    rate = read $ takeWhile (/= ';') $ drop 1 $ dropWhile (/= '=') s :: Int
    children = map (break (== ' ')) $ split (== ',') s

parseInput :: String -> [Node]
parseInput s = map parseNode (lines s)

printNode :: Node -> IO ()
printNode (Node name rate children) = do
  putStrLn name
  putStrLn $ show rate
  putStrLn $ show $ children

printNodes :: [Node] -> IO ()
printNodes [] = return ()
printNodes (h:t) = do
  printNode h
  printNodes t

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where (w, s'') = break p s'