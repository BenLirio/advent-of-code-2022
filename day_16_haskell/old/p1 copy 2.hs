import Control.Monad.Cont


data NodeInfo = NodeInfo { name :: String, rate :: Int } deriving (Show)
parseNodeInfo :: String -> NodeInfo
parseNodeInfo s = do
  let s' = drop 1 $ dropWhile (/= ' ') s
  let name = takeWhile (/= ' ') s'
  let s'' = drop 1 $ dropWhile (/= '=') s'
  let rate = read $ takeWhile (/= ';') s'' :: Int
  NodeInfo name rate

parseNodeChildren :: String -> [String]
parseNodeChildren s = case break (== ',') s of
  (childStr, "") -> [childStr] 
  (childStr, s') -> childID : parseNodeChildren (drop 1 s')
    where childID = takeWhile (/= ' ') childStr



data Node = Node { info :: NodeInfo, children :: [String] } deriving (Show)
parseNode :: String -> Node
parseNode s = do
  let (nodeInfoStr, childrenStr) = break (== ';') s
  let info = parseNodeInfo nodeInfoStr
  let children = parseNodeChildren childrenStr
  Node info children



parseInput :: String -> [Node]
parseInput = map parseNode . lines

main :: IO ()
main = do
  s <- readFile "input_small.txt"
  mapM (putStrLn . show) (parseInput s)
  putStrLn "Done"