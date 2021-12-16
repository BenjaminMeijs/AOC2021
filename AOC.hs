module AOC (runSolvers, ExampleStatus (..), splitBy, bfsOn, bfsOnWithVisited, createSolvers, createSolversNoPrint, 
  parseWith, pLetters, pInt, pDigit, manyGreedy, manyGreedy1, dijkstra) where
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as Char
import qualified Data.PSQueue as PQ
import Data.Char (isSpace)
import qualified GHC.Unicode as Char
type DayNum = Int
data ExampleStatus = ExampleInput | PuzzleInput


createSolvers :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> Int -> ExampleStatus -> IO()
createSolvers parser solverA solverB day example = runSolvers day example parser solverA solverB

createSolversNoPrint :: (String -> a) -> (a->String) -> (a -> String) -> Int -> ExampleStatus -> IO()
createSolversNoPrint parser solverA solverB day example = do
    input <- readInput parser day example
    putStrLn $ solverA input
    putStrLn $ solverB input
    return ()

parseWith :: P.ReadP a -> String -> a
parseWith p s = case [a | (a,t) <- P.readP_to_S p s, null t] of
  [a] -> a
  [] -> error "no parse"
  _ -> error "ambiguous parse"

pLetters :: P.ReadP String
pLetters = P.munch Char.isLetter
pInt :: P.ReadP Int
pInt = read <$> P.munch Char.isDigit
type Digit = Int
pDigit :: P.ReadP Digit
pDigit = Char.digitToInt <$> P.satisfy Char.isDigit
manyGreedy :: P.ReadP a -> P.ReadP [a]
manyGreedy  p = manyGreedy1 p P.<++ return []
manyGreedy1 :: P.ReadP a -> P.ReadP [a]
manyGreedy1 p = (:) <$> p <*> manyGreedy p
 

readInput parser day example = parser <$> readFile ("inputs/day" ++ show day ++ exampleStatusToPathAddition example ++ ".txt" )

runSolvers :: (Show b, Show c) => Int -> ExampleStatus -> (String -> a) -> (a -> b) -> (a -> c) -> IO ()
runSolvers day example parser solverA solverB = do
    input <- readInput parser day example
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


-- Breath first search taken from: 
-- https://topaz.github.io/paste/#XQAAAQBbAwAAAAAAAAAxGYrLUnuO0Fo/becZjJQOKYZXcPSKmNB76DmuoAVlCCI0ZnYTDSCaAJ8mTI5lIVHTSuDlMgAYHgBSWkEJK7dFOK3Kp7XGIDgSSKrpxYX+wdFY9YoqwtNRb6Lkb07qx/u6xWVxBOI5cEXkcJbjoou2F31S0M5YnYxTC7NzD+ecp36C8KLV//KdtK1LoNwrrVU6P+n2Rk8k+JPwfUJQPiOFQBIV78ObhkUXg2XcmCulwx052kjTCREEzikCylpiFya1jUjRNyhg9szMMpn56i/HjMlb6pF80nJiSl+3mLba8kYdtcSe/pmI4N3TwuKtOWMWEO4MC9kkzar9Rp2K2G8Ei+fb9vOS/CzP1cmN8hmqCsVMtueLVXR2CgYs/16CfkAdTKpWnaitBHWHiHwGm1Oo0tngHkvxjJfPu5J1Ek8i0usXSWC7PMGOWr7haPeJZ1syYrQ0UPgPRWlcnS68H/RePGj5BsiPqQFoidMB31jCoCUY1unIFCbDwR/2XnhQ8iZ2KxtK4LX0AX4BWrBVt5zGyQN1vrYK42FnGSWRudJBQqrE9l2Oh/5hiNX/plYcGA==

bfsOnTarget
  :: (Show v, Ord v, Eq v, Ord a, Eq a, Show a)
  => (a -> v) -- repr function for storing search elements in the visited set
  -> (S.Set v -> a -> Seq.Seq a) -- step function, get the next states
  -> a -- start state
  -> (v -> Bool) -- predicate that determines when to stop
  -> [a] -- all states found via BFS
bfsOnTarget repr step start isDone = help S.empty (Seq.singleton start)
 where
  help seen Seq.Empty = []
  help seen (cur Seq.:<| rest)
    | isDone (repr cur)
    = [cur]
    | repr cur `S.member` seen
    = help seen rest
    | otherwise
    = let nbrs   = step seen cur
          queue' = rest Seq.>< nbrs
          seen'  = S.insert (repr cur) seen
      in  cur : help seen' queue'

bfsOn
  :: (Show v, Ord v, Ord a, Eq a, Show a)
  => (a -> v)
  -> (a -> Seq.Seq a)
  -> a
  -> [a]
bfsOn repr step start = bfsOnTarget repr (const step) start (const False)

bfsOnWithVisited
  :: (Show v, Ord v, Ord a, Eq a, Show a)
  => (a -> v)
  -> (S.Set v -> a -> Seq.Seq a)
  -> a
  -> [a]
bfsOnWithVisited repr step start = bfsOnTarget repr step start (const False)


-- taken from: https://topaz.github.io/paste/#XQAAAQAVCAAAAAAAAAA2m8ixrhLu7YJFrd2FLde+PAG1Aui2yN36LCvqT8ZOJmqbLYgEC+r5N94QUS0T9/8dZQVWN/Eq5jVDPAVZTN15zlecQ7+brnMx0LaVIQ1iAdiHKNUpCENXt4qagn0s3gBKcvdJBxmI0ljJMEPVVZgnTltQ0dz3zXfGEk2mTmTsI1Kzr37nQqoyrH1dej8lkPUzs38OpjkFZUs6SfvfHr5fXAShj7NHN8urglvwwXWVTbDpbFtskIB/ij7XYDKF8Q38AMACayG9RH25wTFDDZzwYRMMueAIbCyKAZBwKdKViJ+HpHJAPDUgKQQdJstw7kDjPixoDUGegAZ5swB3qd6mBmRIVEsFwhfYmW9d9hRRtDWYnQdZn+fomHedv1YcO+uDs92bLEtOlXNXg/7aCxb0MJHM/ap28Y+/YyBIf/Z9WNDLv5HG48hghEB8WGB2irKBafQojfsDh96VjpvDeblk2cDGl0QCNFn4FIVclBl4StsDNawqJ2NOGyOpd96QBadmRumlZgPbECyXFykzxcqVdF10ELFCpgKV1ixCcHlu1lDQg0+gMFdHIciApltgePau4eILwryWDvxxvTUW4Roai5y1uy1rRVYhFVcp8lp3k/GaQIxlfX5g0k512j75+zt1p5znuG0q8cYkxL52WVmN338LGUVLsQE5m0uKD5smQrxeyL2mo54zGrnly2mu+OMN+eNbElIAbUbW6Eaz3x88tBNjcdBvoUzKCATLFJgweGS9FxtFQsJWpXidhRYKQQypOBHQopBfk4WnkXfxXY9Y2Ha3kGDZWhYbS3kRN+1Mci1kePc0UlI0OJz6yWdX7CkO5UlJ7TQJ1tt2sBbCNEMrlwOCJBvkEpDBETjXfB7Gz6KbAiodL6kqsG9OHWSM9sPMJDEBXmZcVHlIcf/QF1IWWHe0KKDAEjS6YGVIPoWbqAHduf8lb3oSPwRuADj8fCId854RoKawsp7XxeI+/brFHM4jn+Jv6OzqH5GALyekCskIWmH1X0ePjB0nacDHRmoO8m0nE3ovAhg7MGJS+G08JGPtCjqxNieiOPCPq1K5njyJIrjHjbuSYN/MA0wUhcWD6Ct7XJYJ7LklBJe0YNNSUwnb+TI7iCowt8B0k4lhj9Csnn02qSk6ZMtIhXL6N/5TSj7Spr1UlnVkkv0jzQBMr01AG1FRO7pW4UW/f2musf411LK7mlUln4xZJKhTScdDH23qJy81a1yUFjH6NQUA29MOwPtyHDqu0PHdDLp6kCtax5lIJtnj4P4Lyb3HPYdx5loKhMrzV2XFRd0Hl8W8odBdCf/v2i07uhvctC+MZGNZvZbwrBQ69gImmGOzOlC+QsFYhom24iGN3hbkHkeVbxzfkaS2Rjfcb03+RS/iLxqVSuFdAdY2JR7PxDHh1Y+5lpt8gkL/5HF8FA==
-- Generalized version of Dijkstra's shortest paths algorithm.
dijkstra
  :: (Show v, Ord v, Eq v, Ord a, Eq a, Show a, Foldable f, Ord d, Num d)
  => (a -> v) -- repr function for storing search elements in the visited set
  -> (a -> f (a, d)) -- find neighbors and distance
                          -- NOTE: the Int here is total g(v)+h(v),
                          -- so set h(v)>0 if using A* with a consistent
                          -- heuristic.
  -> (a, d) -- start state and start distance
  -> (v -> Bool) -- predicate for when we're done
  -> [(a, d)]
dijkstra repr step (start, p) isDone = help S.empty (PQ.singleton start p)
 where
  help seen queue = case PQ.findMin queue of
    Nothing                -> []
    Just (cur PQ.:-> dist) -> if isDone (repr cur)
      then [(cur, dist)]
      else if repr cur `S.member` seen
        then help seen (PQ.deleteMin queue)
        else
          let enq q (a, dist') = PQ.insertWith min a (dist + dist') q
              queue' = foldl enq (PQ.deleteMin queue) (step cur)
              seen'  = S.insert (repr cur) seen
          in  (cur, dist) : help seen' queue'