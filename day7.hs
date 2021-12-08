import Data.List
main = do
    inputText <- readFile "inputs/day7.txt" 
    let input = parseInput inputText
    print $ solveA input
    print $ solveB input

parseInput :: String -> [Int]
parseInput input = map read $ wordsWhen (==',') input

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Turns out this is just the median
solveA = lowestFuelCostBy fuellForPositionA

lowestFuelCostBy :: CostCalculator -> [Int] -> Int
lowestFuelCostBy cc input = snd . minimumBy (\(_,c1) (_,c2) -> compare c1 c2) $ fuellPerPosition cc input

type Cost = Int
type Pos  = Int
fuellPerPosition :: CostCalculator ->  [Int] -> [(Pos,Cost)]
fuellPerPosition cc input = [ (p, cc input p) | p <- [start .. end]]
    where start = minimum input
          end   = maximum input

type CostCalculator = [Int] -> Pos -> Cost
fuellForPositionA :: CostCalculator
fuellForPositionA input pos = fuellForPositionBy (\x -> abs (x-pos)) input

fuellForPositionBy :: (Pos -> Cost) -> [Int] -> Cost
fuellForPositionBy costFunction input = sum $ map costFunction input



-- Turns out this is just the Mean
solveB = lowestFuelCostBy fuellForPositionB

fuellForPositionB :: CostCalculator
fuellForPositionB input pos = fuellForPositionBy (\x -> sumBefore (abs (x - pos))) input

sumBefore :: Int -> Int
sumBefore n = ((n + 1)*n) `div` 2
