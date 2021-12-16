
module Day14(solvers) where
import AOC
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import Text.ParserCombinators.ReadP as P
import qualified Data.Char as Char 
import Data.Maybe (maybe)

solvers = createSolvers parseInput2 solveA solveB 14


-- PARSING
type Rules = M.Map (Char,Char) Char
parseInput :: String -> (String, Rules)
parseInput input = (start, rules)
    where (start:_:rs) = lines input
          rules = M.fromList $ map parseRule rs
          parseRule r = let [[a,b],_,[right]] = words r in ((a,b),right)

-- Later added for practice with parser combinators
parseInput2 = parseWith pInput
  where pInput = (,) <$> pStart <* skipSpaces <*> pRules 
        pStart = munch Char.isLetter 
        pRules = M.fromList <$> many (pRule <* skipSpaces)
        pRule  = (,) <$> pPair <* string " -> " <*> get
        pPair  = (,) <$> get <*> get

-- HELPERS FOR BOTH PARTS

mostCommon = MS.foldOccur (\v i (b, j) -> if i > j then (v,i) else (b,j)) (undefined, minBound)
leastCommon = MS.foldOccur (\v i (b, j) -> if i < j then (v,i) else (b,j)) (undefined, maxBound)
mostMinusLeast x = snd (mostCommon x) - snd (leastCommon x) 

-- PART A

solveA (polymer,rules) = mostMinusLeast . MS.fromList $ allSteps polymer rules !! 10

allSteps polymer rules = iterate (applyInsertions rules) polymer

applyInsertions rules polymer = concatMap f pairs
  where pairs = zip polymer (tail polymer ++ ['-'])
        f pair@(x,_) = x : toAdd pair
        toAdd pair = maybe [] pure (M.lookup pair rules)

-- PART B (Already expected something similair to Day 6)
solveB (polymer, rules) = mostMinusLeast . pairsToElements . stepN 40 rules $ createPairs polymer

stepN n rules pairs = iterate (step rules) pairs !! n

type Pair = (Char, Char)
type Pairs = MS.MultiSet Pair
step :: Rules -> Pairs -> Pairs
step rules = MS.concatMap (produces rules)

produces :: Rules -> Pair -> [Pair]
produces rules pair@(a,b) = maybe [pair] (\n -> [(a,n), (n,b)]) $ M.lookup pair rules

createPairs polymer = MS.fromList $ zip polymer (tail polymer ++ ['-'])
pairsToElements = MS.map fst