module Day1 where
solve = do
    inputText <- readFile "inputs/day1.txt" 
    let input = map read . lines $ inputText
    print $ solveB input
    
solveA :: [Int] -> Int
solveA xs = length .  filter (\(a, b) -> b > a)
            $ zip xs (tail xs)

solveB :: [Int] -> Int
solveB xs = solveA . map (\(a, b, c) -> a + b + c) 
            $ zip3 xs (tail xs) (tail $ tail xs)