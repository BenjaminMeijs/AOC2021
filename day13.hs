module Day13(solvers) where
import AOC
import Data.List
import qualified Data.Set as S
import Data.Bifunctor (bimap)

solvers = createSolversNoPrint parseInput solveA solveB 13

type Pos = (Int,Int)
type Grid = S.Set Pos
data Fold = Xfold Int | Yfold Int deriving (Show, Eq, Ord)

parseInput :: String -> (S.Set Pos, [Fold])
parseInput input = (dots, folds)
  where  (d,(_:f)) = span (/="") $ lines input
         dots  =S.fromList $ map parseDot d
         folds = map parseFold f
         parseDot dot = let [x,y] = splitBy (==',') dot in (read x, read y)
         parseFold fold = case words fold !! 2 of
             ('x':'=':i) -> Xfold $ read i
             ('y':'=':i) -> Yfold $ read i

solveA (grid, (fold:_)) = show . S.size $ applyFold fold grid

applyFold :: Fold -> Grid -> Grid
applyFold fold = S.map (foldedPos fold)

foldedPos :: Fold -> Pos -> Pos
foldedPos (Xfold i) (x,y) = (i - abs (x - i), y)
foldedPos (Yfold i) (x,y) = (x, i - abs (y - i))

solveB = prettyPrintGrid . uncurry applyAllFolds 

prettyPrintGrid :: Grid -> String
prettyPrintGrid grid = unlines [[ if (x,y) `S.member` grid then '█' else ' ' --Later added the █ symbol if found on /r/haskell
                                | x <- [0 .. width]] | y<-[0..height]]
  where (width, height) = bimap maximum maximum . unzip $ S.toList grid  


applyAllFolds :: Grid -> [Fold] -> Grid
applyAllFolds = foldl (flip applyFold) -- Amazing what pointfree style can do