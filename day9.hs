import AOC
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import qualified Data.Ix as Ix
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord

main = runSolvers 9 PuzzleInput parseInput solveA solveB 

parseInput :: String -> A.Array (Int,Int) Int 
parseInput input = A.listArray ((0,0),(width, height)) (concat $ transpose listList)
    where listList :: [[Int]]
          listList = map (map digitToInt) (lines input)
          width  = subtract 1 . length $ head listList
          height = subtract 1 $ length listList

type Pos = (Int, Int)
type Grid = A.Array Pos Int

solveA = sum . map ((+1) . snd) . findLowestPoints

findLowestPoints :: Grid -> [(Pos, Int)]
findLowestPoints grid = filter isLowest $ A.assocs grid
  where isLowest (pos, n) = all (>n) $ getNeighboursValue grid pos 

getNeighboursValue :: Grid -> Pos -> [Int]
getNeighboursValue grid pos = map snd $ getNeighbours grid pos

getNeighbours :: Grid -> Pos -> [(Pos, Int)]
getNeighbours grid (x,y) = catMaybes [up, down, left, right]
  where up    = construct (x,y-1)
        down  = construct (x,y+1)
        left  = construct (x-1, y)
        right = construct (x+1, y)
        construct pos = (\v -> (pos, v)) <$> grid !? pos 


(!?) :: Grid  -> Pos -> Maybe Int
(!?) array index
    | Ix.inRange (A.bounds array) index = Just $ array A.! index
    | otherwise = Nothing

solveB = product . getBiggest3BassinsSize

getBiggest3BassinsSize = take 3 . sortOn Down . map length 
                        . group . sort . A.elems . getFlowsTowards

getFlowsTowards :: Grid -> A.Array Pos Pos
getFlowsTowards grid = result
  where result = gridMap flowsToEnd grid
        flowsToEnd :: Pos -> Int -> Pos
        flowsToEnd pos _ 
          | grid A.! pos == 9  = pos
          | pos == flowsTo pos = pos 
          | otherwise          = result A.! flowsTo pos -- Yep, this is relying on lazyness
        flowsTo :: Pos -> Pos
        flowsTo pos = fst $ minimumBy (\(_, a) (_, b) -> compare a b) ((pos, grid A.! pos) : getNeighbours grid pos)

gridMap :: Ix.Ix i => (i -> v -> e) -> A.Array i v  -> A.Array i e
gridMap f grid = A.array (A.bounds grid) $ map (\(pos, value) -> (pos, f pos value)) (A.assocs grid)
  