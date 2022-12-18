import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
i = 3.0

-- ============== Util ================

-- Stackoverflow
maximum :: Ord a => [a] -> a
maximum = foldr1 (\x y ->if x >= y then x else y)
minimum :: Ord a => [a] -> a
minimum = foldr1 (\x y ->if x < y then x else y)
sum :: [Int] -> Int
sum [] = 0
sum (h:t) = h + Main.sum t

-- Copilot
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (h:t) = concatMap (insert h) (permutations t)
  where insert :: a -> [a] -> [[a]]
        insert x [] = [[x]]
        insert x (h:t) = (x : h : t) : map (h :) (insert x t)

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
rateOf :: Map String Node -> String -> Int
rateOf graph loc = case Map.lookup loc graph of Just n -> rate n
graphDist :: Map String Node -> String -> String -> Int
graphDist graph loc1 loc2 | loc1 == loc2 = 0
graphDistAux :: Map String Node -> Map String Bool -> String -> String -> Int
graphDistAux _ _ loc1 loc2 | loc1 == loc2 = 0
graphDistAux graph visited loc1 loc2 = do
  let node = case Map.lookup loc1 graph of Just n -> n
  let (Node name rate children) = node
  let visited' = Map.insert loc1 True visited
  let children' = filter (\c -> case Map.lookup c visited' of Just b -> True; _ -> False) children
  1 + (Main.minimum $ map (graphDistAux graph visited' loc1) children')





makeGraph :: [Node] -> Map String Node
makeGraph [] = Map.empty
makeGraph (h:t) = Map.insert (name h) h (makeGraph t)

-- ============== Solver ==============
data State = State {
  step :: Int,
  loc :: String,
  open :: [String],
  score :: Int
} deriving (Show)
incStep :: State -> State
incStep (State step loc open score) = State (step + 1) loc open score
setLoc :: State -> String -> State
setLoc (State step _ open score) loc = State step loc open score
openLoc :: State -> String -> State
openLoc (State step loc open score) loc' = State step loc (loc' : open) score
addScore :: State -> Int -> State
addScore (State step loc open score) delta = State step loc open (score + delta)

initialState = State 0 "OK" [] 0
solve :: Map String Node -> State -> Int
solve graph (State 10 _ _ score) = score
solve graph state = do
  let (State step loc open score) = state
  let node = case Map.lookup loc graph of Just n -> n
  let (Node name rate children) = node
  let deltaScore = Main.sum $ map (rateOf graph) open
  let state' = addScore (incStep state) deltaScore
  let openStates = if elem loc open then [] else if rate == 0 then [] else [openLoc state' loc]
  let travelStates = map (setLoc state') children
  let nextStates = openStates ++ travelStates
  Main.maximum $ map (solve graph) nextStates


scorePermutation :: Map String Node -> [Node] -> Int
scorePermutation graph nodes = scorePermutationAux graph 0 [] nodes

scorePermutationAux :: Map String Node -> Int -> [Node] -> [Node] -> Int
scorePermutationAux _ _ _ _ = 0

solveV2 :: [Node] -> Int
solveV2 nodes = do
  let graph = makeGraph nodes
  let nonZeroNodes = filter (\n -> rate n /= 0) nodes
  Main.maximum $ map (scorePermutation graph) (permutations nonZeroNodes)



-- ============== Main ==============
main :: IO ()
main = do
  s <- readFile "input_small.txt"
  putStrLn $ show $ solveV2 $ parse $ tokenize s