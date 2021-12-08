module AOC (runSolvers, ExampleStatus (..), splitBy) where
type DayNum = Int
data ExampleStatus = ExampleInput | PuzzleInput

runSolvers :: Show b => Int -> ExampleStatus -> (String -> a) -> (a -> b) -> (a -> b) -> IO ()
runSolvers day example parser solverA solverB = do
    input <- parser <$> readFile ("inputs/day" ++ show day ++ exampleStatusToPathAddition example ++ ".txt" )
    print $ solverA input
    print $ solverB input
    return ()

exampleStatusToPathAddition ExampleInput = "example"
exampleStatusToPathAddition PuzzleInput = ""




splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s = case dropWhile p s of
                      [] -> []
                      s' -> w : splitBy p s''
                            where (w, s'') = break p s'