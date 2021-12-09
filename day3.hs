module Day3 where
solve = do
    inputText <- readFile "inputs/day3.txt" 
    let input = lines $ inputText
    print $ solveA input
    print $ solveB input

solveA input = gamma * epsilon
    where gammaBin = map countMost1 $ slices input
          gamma = binToInt gammaBin
          epsilon = binToInt $ complement gammaBin

countLeast0 :: String -> Char
countLeast0 xs = if length (filter (=='0') xs) <=  length (filter (=='1') xs)
                 then '0'
                 else '1'

countMost1 :: String -> Char
countMost1 xs = if length (filter (=='0') xs) > length (filter (=='1') xs)
                 then '0'
                 else '1'

complement :: String -> String
complement = map (\x -> if x == '1' then '0' else '1')


solveB input = binToInt co2 * binToInt o2
    where o2  = solveBHelper countMost1 input ""
          co2 = solveBHelper countLeast0 input ""

-- This is not the pretiests solution
solveBHelper :: (String -> Char) -> [String] -> String -> String
solveBHelper _ [x] output = output ++ x
solveBHelper counter input output = solveBHelper counter next (output ++ [common]) 
    where sepped = map (\(h:ts) ->  (h, ts)) input
          common = counter $ map fst sepped
          next = map snd $ filter (\x -> fst x ==common) sepped


slices :: [String] -> [String]
slices xs = [map (!! n) xs | n <- [0 .. length (head xs)-1]]

binToInt :: String -> Int
binToInt [] = 0
binToInt xs = fst $ foldr f (0,1) xs 
  where f c (sum, pow) = case c of
            '1' -> (sum + pow, pow*2)
            '0' -> (sum      , pow*2)
            _ -> error "bin To Int expects a string containing only 1's and 0's"