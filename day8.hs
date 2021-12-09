module Day8 where
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad ( (<$!>) )
import AOC

solve = runSolvers 8 PuzzleInput parseInput solveA solveB 


type Input = [String]
type Output = [String]
type Puzzle = (Input, Output)

parseInput :: String -> [Puzzle]
parseInput input = map ((\[l,r]-> (words l, words r)) . splitBy (=='|')) (lines input)


-- Simplest part 1 untill now?
solveA :: [Puzzle] -> Int
solveA = sum . map (count_1_4_7_8 . snd) 

count_1_4_7_8 :: [String] -> Int
count_1_4_7_8 = length . filter f . map length
  where f i = i `elem` [2, 4, 3, 7]


getDecoder :: [String] -> M.Map (S.Set Char) Int
getDecoder input = M.fromList $ zip [zero,one,two,three,four,five,six,seven,eight,nine] [0..9]
  where -- Helpers
        filterLength n = filter((==n) .length) input
        getManyWithLength = map S.fromList . filterLength
        getFirstWithLength = head . getManyWithLength
        combineAndFind pred combiner options = head $ filter (pred . S.union combiner) options
        -- 1, 4, 7, 8
        one   = getFirstWithLength 2
        four  = getFirstWithLength 4
        seven = getFirstWithLength 3
        eight = getFirstWithLength 7
        -- 0, 6, 9
        zeroSixNine = getManyWithLength 6
        six   = combineAndFind (==eight) one zeroSixNine
        nine  = combineAndFind (/=eight) four zeroSixNine 
        zero  = combineAndFind (==eight) five zeroSixNine
        -- 2, 3, 5
        twoThreeFive = getManyWithLength 5
        two   = combineAndFind (==eight) four twoThreeFive
        five  = combineAndFind (==nine) one twoThreeFive
        three = combineAndFind (==nine) five twoThreeFive

solveSingleDisplay :: [String] -> [String] -> Int
solveSingleDisplay input =  digitListToDecimal . map ((decoder M.!) . S.fromList)
    where decoder = getDecoder input

digitListToDecimal :: [Int] -> Int
digitListToDecimal = foldl' (\r v -> 10*r + v) 0

-- Took me a while to solve the puzzle, but its a nice solution.
solveB = sum . map (uncurry solveSingleDisplay) 