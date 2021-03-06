module Day9 (solvers) where
import AOC ( createSolvers, ExampleStatus(PuzzleInput) )
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import qualified Data.Ix as Ix
import qualified Data.MultiSet as MS
import qualified Data.PQueue.Max as PQM
import Data.Char ( digitToInt )
import Data.List ( sort, minimumBy, group, sortOn, transpose )
import Data.Maybe ( catMaybes )
import Data.Ord ( Down(Down) )

solvers = createSolvers parseInput solveA solveB 

parseInput :: String -> A.Array (Int,Int) Int 
parseInput input = A.listArray ((0,0),(width, height)) (concat $ transpose listList)
    where listList :: [[Int]]
          listList = map (map digitToInt) (lines input)
          width  = subtract 1 . length $ head listList
          height = subtract 1 $ length listList

type Pos = (Int, Int)
type Grid = A.Array Pos Int

solveA :: Grid -> Int
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

solveB :: Grid -> Int
solveB = product . getBiggest3BassinsSize

-- New implimentation using Multiset and Priority queue 
getBiggest3BassinsSize :: Grid -> [Int]
getBiggest3BassinsSize = map (snd . unBassin) .  PQM.take 3 . PQM.fromList . map Bassin 
                     . MS.toOccurList . MS.fromList . A.elems . getFlowsTowards

newtype Bassin = Bassin {unBassin ::(Pos, MS.Occur)} deriving Eq
instance Ord Bassin where
  compare (Bassin (_, a)) (Bassin (_, b)) = compare a b


getFlowsTowards :: Grid -> A.Array Pos Pos
getFlowsTowards grid = result
  where result = gridMap flowsToEnd grid -- Lazyness will be used...
        flowsToEnd :: Pos -> Int -> Pos
        flowsToEnd pos _ 
          | grid A.! pos == 9  = pos -- We can model 9's as being their own little basin
          | pos == flowsTo pos = pos -- Lowest point of basin
          | otherwise          = result A.! flowsTo pos -- Yep, this is the part relying on lazyness
        flowsTo :: Pos -> Pos
        flowsTo pos = fst $ minimumBy (\(_, a) (_, b) -> compare a b) ((pos, grid A.! pos) : getNeighbours grid pos)

-- Not the most efficient or beautiful implementation, but it works
gridMap :: Ix.Ix i => (i -> v -> e) -> A.Array i v  -> A.Array i e
gridMap f grid = A.array (A.bounds grid) $ map (\(pos, value) -> (pos, f pos value)) (A.assocs grid)
  