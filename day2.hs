module Day2 where
solve = do
    inputText <- readFile "inputs/day2.txt" 
    let input = map parseCommand . lines $ inputText
    print $ solveA input
    print $ solveB input

data Command = Forward Int
             | Down Int
             | Up Int
parseCommand :: String -> Command
parseCommand s = case dir of
    "forward" -> Forward num
    "down" -> Down num
    "up" -> Up num
    _ -> error "There was a command that could not be parsed"
    where [dir, numString] = words s
          num = read numString

solveA :: [Command] -> Int
solveA input = uncurry (*) $ foldl f (0,0) input
    where f (hor, dep) com = case com of
                                (Forward n) -> (hor + n, dep)
                                (Down n) -> (hor, dep + n)
                                (Up n) -> (hor, dep - n)
       
solveB input = (\(h,d,_)-> h*d) $ foldl f (0, 0, 0) input
    where f (hor, dep, aim) com = case com of
                                (Forward n) -> (hor + n, dep + n * aim, aim)
                                (Down n) -> (hor, dep, aim + n)
                                (Up n) -> (hor, dep, aim - n)

