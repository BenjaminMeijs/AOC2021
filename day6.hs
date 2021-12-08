import Data.List 
import Data.Maybe
import qualified Data.Map as M
import qualified Data.MultiSet as MS

main = do
    inputText <- readFile "inputs/day6.txt" 
    let input = parseInput inputText
    print $ solveA input
    print $ solveB input


parseInput :: String -> [Int]
parseInput = map read . wordsWhen (==',') 

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

stepDayGen :: [Int] -> [Int]
stepDayGen gen = newGen ++ sameGen
    where sameGen = map stepDaySingle gen
          newGen = replicate (length $ filter (==0) gen) 8

stepDaySingle :: Int -> Int
stepDaySingle x
  | x == 0    = 6
  | otherwise = x - 1


-- Brute forcing. I'm not afraid of the big O, what could go wrong?
solveA input = length $ iterate' stepDayGen input !! 80 

-- Ah that can go wrong, back to the drawing board
solveB input = sum . M.elems $ 
              iterate' stepDayGen2 (inputToMap input) !! 256

inputToMap input = M.fromList . concatMap (\xs -> zip xs (repeat $ length xs) ) . group $ sort input

stepDayGen2 :: M.Map Int Int -> M.Map Int Int
stepDayGen2 input = oldGen `M.union` newGen
    where   oldGenNewCreated = fromMaybe 0 $ M.lookup 0 input
            f Nothing = Just oldGenNewCreated
            f (Just n) = Just $ n + oldGenNewCreated
            oldGen = M.alter f 6 . M.delete (-1) $ M.mapKeys (subtract 1) input
            newGen
              | oldGenNewCreated == 0 = M.empty
              | otherwise = M.singleton 8 oldGenNewCreated

--Later i saw a solution using MultiSet, this looked so beautifull i just wanted to try it for myself
        