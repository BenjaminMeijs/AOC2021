module Day15(solvers) where
import AOC
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Ix as Ix
import qualified Debug.Trace as T
import Control.Monad.State.Lazy
import Data.List
import Data.Char
import Data.Maybe

solvers = createSolvers parseInput solveA solveB 15


-- parseInput :: String -> A.Array (Int,Int) Int
parseInput input = A.listArray ((0,0),(width, heigth)) . concat . transpose $ listList
  where listList = map (map digitToInt) . lines $ input
        heigth = subtract 1 $ length listList
        width  = subtract 1 . length $ head listList

solveA input = "nope"-- (A.! goal) . distances $ execState (dijkstraStep input (0,0)) (initialState input)
  where goal = getGoal input
initialState chart = DijkstraState distances' unvisited'
  where unvisited' = S.fromList $ A.indices chart
        distances' = A.array (A.bounds chart) [if i == (0,0) then (i,0) else (i,inf) | i <- A.indices chart]

getGoal chart = let (_,(x,y)) = A.bounds chart in (x,y)

inf :: Int
inf = 10000000

data DijkstraState = DijkstraState {distances :: A.Array (Int,Int) Int, unvisited :: S.Set (Int,Int)}
      deriving (Show, Eq, Ord)

type Chart =  A.Array (Int,Int) Int
dijkstraStep :: Chart -> (Int,Int) -> State DijkstraState ()
dijkstraStep chart thisNode = do
  unvisited' <- gets unvisited
  let neighbours = filter (`S.member` unvisited') (getNeighbours chart thisNode)
  distances' <- gets distances
  let thisDistance = distances' A.! thisNode
  let tentativeDistances = getTentativeDistances chart distances' thisDistance neighbours
  modify $ updateDistances (A.// tentativeDistances)
  modify $ updateUnvisited (S.delete thisNode)
  done <- gets (S.null . unvisited)
  if done then return ()
  else do
    next <- gets $ fst . findNext
    dijkstraStep chart next


findNext (DijkstraState distances' unvisited') = minimumBy (\(_,a) (_,b) -> compare a b)
  . filter (\(i,_) -> S.member i unvisited') $ A.assocs distances'

updateDistances f = updateDijkstraState f id
updateUnvisited = updateDijkstraState id
updateDijkstraState f g (DijkstraState d u) = DijkstraState (f d) (g u)

getTentativeDistances chart distances' thisDistance = map (\i -> (i, f i))
  where f n = getTentativeDistance (distances' A.! n) thisDistance (edgeCost n)
        edgeCost n = chart A.! n

getTentativeDistance oldCost neighbourCost edgeCost
  | oldCost > neighbourCost + edgeCost = neighbourCost + edgeCost
  | otherwise = oldCost

getNeighbours chart (x,y) = mapMaybe construct [(-1,0), (1,0), (0,1), (0,-1)]
  where construct p@(dx,dy)
          | Ix.inRange bs p = Just (x+dx, y+dy)
          | otherwise       = Nothing
        bs = A.bounds chart

getNeighboursWithValue chart (x,y) = mapMaybe construct [(-1,0), (1,0), (0,1), (0,-1)]
  where construct (dx,dy) = (\v -> ((x+dx,y+dy), v)) <$> arrayLookup chart (x+dx, y+dy)

arrayLookup arr index
  | Ix.inRange (A.bounds arr) index = Just $ arr A.! index
  | otherwise = Nothing


-- 01|23|45|67|89
-- 

-- testBuildExtrapolatedChart = buildExtrapolatedChart 2 (A.listArray ((0,0),(1,1)) [1,3,2,4])
solveB input = solveX $ buildExtrapolatedChart 5 input
buildExtrapolatedChart :: Int -> A.Array (Int, Int) Int -> A.Array (Int,Int) Int
buildExtrapolatedChart n chart = A.array newBounds $ concatMap construct dzs
  where (ow, oh) = let (ow', oh') = snd $ A.bounds chart in (ow'+ 1 , oh' + 1)
        assocs = A.assocs chart
        newBounds = ((0,0),(ow*n - 1, oh*n - 1))
        construct (dx,dy) = map (\((x,y),v) -> ((x+ (ow*dx), y+(oh*dy)), increaseN (dx + dy) v)) assocs
        dzs = [(dx, dy)| dx <- [0..n-1], dy <- [0..n-1]]

increaseN n v = let x = (v + n) `mod` 9 in if x == 0 then 9 else x

-- Using found dijkstra algorithm
solveX chart = map snd . filter ((==goal) . fst) $ 
              dijkstra id next ((0,0), 0) (==goal)
  where goal = getGoal chart
        next = getNeighboursWithValue chart