module Day11(solvers) where
import AOC
import qualified Data.Array as A
import qualified Data.Ix as Ix
import Data.List
import Data.Char (digitToInt)
import Control.Monad.State.Lazy
import qualified Data.MultiSet as MS
import qualified Data.Set as S
import Text.ParserCombinators.ReadP as P

solvers = createSolvers parseInput solveA solveB 11

-- parseInput :: String -> A.Array (Int,Int) Int 
parseInput = A.listArray ((0,0),(9,9)) .  concat . transpose . map (map parseOctopus) . lines
parseOctopus :: Char -> Octopus
parseOctopus = Charging . digitToInt

type Pos = (Int,Int)
type Grid = A.Array Pos Octopus
data Octopus = Flashed | Charging Int deriving (Show, Eq, Ord)


-- Inspired by a solution from the Haskell subreddit
solveA = id -- sum . evalState (replicateM 100 step)



step :: State Grid Int
step = do 
    modify increaseAllEnergy
    flashes <- flash
    modify unflashAll
    pure flashes
    

flash :: State Grid Int
flash = do 
    toFlash <- gets getNewFlashersPos
    let thisFlashes = S.size toFlash 
    if thisFlashes == 0
        then pure 0
        else 
            do  
                let toIncrease = MS.fromList $ concatMap neighboursPos (S.toList toFlash) 
                modify (setToFlashed toFlash)
                modify (increaseEnergy toIncrease) 
                recFlashes <- flash
                pure $ thisFlashes + recFlashes

getNewFlashersPos = S.fromList . map fst . filter (isNewFlasher . snd) . A.assocs
  where isNewFlasher (Charging i) = i >= 10
        isNewFlasher _            = False


increaseAllEnergy = gridMap f
  where f (x,y) (Charging i) = Charging (i+1)

increaseEnergy ms = gridMap f
  where f p (Charging i) = Charging $ i + MS.occur p ms 
        f _ x = x

setToFlashed poss = gridMap f
  where f p (Charging i)
          | S.member p poss = Flashed
          | otherwise      = Charging i
        f _ x = x

unflashAll = gridMap f
  where f _ Flashed  = Charging 0
        f _ x        = x

-- Not the most efficient or beautiful implementation, but it works
gridMap :: Ix.Ix i => (i -> v -> e) -> A.Array i v  -> A.Array i e
gridMap f grid = A.array (A.bounds grid) $ map (\(pos, value) -> (pos, f pos value)) (A.assocs grid)

-- withinDayStep :: Grid -> S.Set Pos -> (MS.MultiSet Pos, S.Set Pos )
-- withinDayStep grid flashed = grid 

neighboursPos :: Pos -> [Pos]
neighboursPos (x,y) = [pos | dx <- [-1..1], dy <- [-1..1], let pos = (x+dx, y+dy), pred pos]
  where pred p = p/=(x,y) && Ix.inRange ((0,0),(9,9)) p 
        

solveB = fmap (+1) . elemIndex 100 . evalState (replicateM 1000 step) --1000 is just an arbitrary (but large) number