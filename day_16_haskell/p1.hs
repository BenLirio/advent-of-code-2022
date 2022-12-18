import Data.Map (Map)
import qualified Data.Map as Map

-- ============== Tokenizer ==============

data Token = ID String | Num Int | Word String | Semi | Comma | Eq | Nl deriving (Show)
isNl :: Token -> Bool
isNl Nl = True
isNl _ = False 
isSeparator :: Char -> Bool
isSeparator c = c == ' ' || c == ',' || c == ';' || c == '=' || c == '\n'
isNotSeparator :: Char -> Bool
isNotSeparator = not . isSeparator
isCap :: Char -> Bool
isCap c = c >= 'A' && c <= 'Z'
isID :: String -> Bool
isID s = all isCap s && length s == 2
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
isNum :: String -> Bool
isNum s = all isDigit s

tokenize :: String -> [Token]
tokenize "" = []
tokenize (' ':t) = tokenize t
tokenize (';':t) = Semi : tokenize t
tokenize (',':t) = Comma : tokenize t
tokenize ('=':t) = Eq : tokenize t
tokenize ('\n':t) = Nl : tokenize t
tokenize s = do 
  let (word, rest) = span isNotSeparator s
  let token = if isID word then ID word else if isNum word then Num (read word :: Int) else Word word
  token : tokenize rest

-- ============== Parser ==============

data Node = Node { name :: String, rate :: Int, children :: [String] } deriving (Show)
parseLine :: [Token] -> Node
parseLine = parseLine0
parseLine0 :: [Token] -> Node
parseLine0 (ID name : rest) = parseLine1 name rest
parseLine0 (_ : rest) = parseLine0 rest
parseLine1 :: String -> [Token] -> Node
parseLine1 name (Num val : rest) = parseLine2 name val [] rest
parseLine1 name (_ : rest) = parseLine1 name rest
parseLine2 :: String -> Int -> [String] -> [Token] -> Node
parseLine2 name val children [] = Node name val children
parseLine2 name val children (ID child : rest) = parseLine2 name val (child : children) rest
parseLine2 name val children (_ : rest) = parseLine2 name val children rest
parse :: [Token] -> [Node]
parse [] = []
parse tokens = do
  let (line, rest) = break isNl tokens
  parseLine line : parse (drop 1 rest)

-- ============= Graph ===============

makeGraph :: [Node] -> Map String Node
makeGraph [] = Map.empty
makeGraph (h:t) = Map.insert (name h) h (makeGraph t)

graphDist :: Map String Node -> String -> String -> Int
graphDist graph from to = do
  graphDistAux graph Map.empty [from] 0 to
graphDistAux :: Map String Node -> Map String Int -> [String] -> Int -> String -> Int
graphDistAux _ _ [] _ _ = -1
graphDistAux graph visited queue depth to = do
  let queueNodes = map (graph Map.!) queue
  let queueChildren = concatMap children queueNodes
  let validChildren = filter (\n -> not $ Map.member n visited) queueChildren
  case elem to validChildren of
    True -> depth + 1
    False -> do
      let visited' = foldl (\m n -> Map.insert n (depth+1) m) visited validChildren
      graphDistAux graph visited' validChildren (depth+1) to

-- ============== Solve =============

data State = State {
  loc :: String,
  opened :: [String],
  visited :: Map String Bool,
  delta :: Int,
  score :: Int,
  step :: Int
} deriving (Show)
initialState = State {
  loc = "AA",
  opened = [],
  visited = Map.empty,
  delta = 0,
  score = 0,
  step = 0
}
maximumStep = 30

actions :: Map String Node -> State -> [State]
actions _ (State _ _ _ _ _ step) | step > maximumStep = []
actions graph state = do
  let (State loc opened visited delta score step) = state
  let visited' = Map.insert loc True visited
  let state' = state {
    visited = visited',
    step = step + 1,
    score = score + delta
  }
  let (Node _ rate children) = graph Map.! loc
  let validChildren = filter (\c -> Map.notMember c visited') children
  let states = [ state' { loc = child } | child <- validChildren ]
  let states' = if (rate == 0) || (elem loc opened) then states else state' {
    opened = loc : opened,
    visited = Map.empty,
    delta = delta + rate
  } : states
  states'

solve :: Map String Node -> State -> Int
solve graph  state = do
  let (State _ _ _ delta score _) = state
  case actions graph state of
    [] -> score - delta
    states -> maximum $ map (solve graph) states


-- ============== Main ==============
main :: IO ()
main = do
  s <- readFile "input.txt"
  let graph = makeGraph $ parse $ tokenize s
  putStrLn $ show $ solve graph initialState