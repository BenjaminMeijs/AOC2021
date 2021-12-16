module Day12(solvers) where
import AOC
import qualified Data.MultiMap as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Text.ParserCombinators.ReadP as P
import Data.List

solvers = createSolvers parseInput2 solveA solveB 12

parseInput = M.fromList . toBidirect . map getPair . lines
  where getPair line = let [a,b] = splitBy(=='-') line in (a,b)
        toBidirect = concatMap (\(a,b) -> [(a,b), (b,a)])

-- Later added to practice parser combinators
parseInput2 = parseWith pInput
  where pInput = toBidirect <$> pPairs
        pPairs = many (pPair <* skipSpaces)
        pPair = (,) <$> pLetters <* char '-' <*> pLetters
        toBidirect = M.fromList . concatMap (\(a,b) -> [(a,b), (b,a)])


isSmall (s:_) = 'a' <= s && s <= 'z'
isLarge = not . isSmall

solveX input mayVisit = fmap fst . find (uncurry (==)) $ zip answers (tail answers)
  where answers = map length $ allGens input mayVisit

solveA input = solveX input mayVisitA
  
allGens graph mayVisit = iterate nextGen [["start"]]
  where nextGen xs = concatMap addNextNode xs
        addNextNode xs@("end":_)   = [xs]
        addNextNode (xs@(x:_)) = map (\a -> a : xs) (nextOptions x xs)
        nextOptions x xs = filter (mayVisit xs) $ graph M.! x


mayVisitA _ "start" = False
mayVisitA path option = isLarge option ||  notElem option path

-- I mean, why even try to find an efficient solution?
solveB input = solveX input mayVisitB

mayVisitB _ "start" = False
mayVisitB path option = not (isSmall option)
  || MS.notMember option set 
  || not (multiSetAnyOccur (\v i -> isSmall v && i >= 2) set)
  where set = MS.fromList path

multiSetAnyOccur :: (a -> MS.Occur -> Bool) -> MS.MultiSet a -> Bool 
multiSetAnyOccur pred = MS.foldOccur (\v i r -> r || pred v i) False 