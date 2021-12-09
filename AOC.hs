module AOC (runSolvers, ExampleStatus (..), splitBy, bfsOn, createSolvers) where
import qualified Data.Sequence as Seq
import qualified Data.Set as S
type DayNum = Int
data ExampleStatus = ExampleInput | PuzzleInput


createSolvers :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> Int -> ExampleStatus -> IO()
createSolvers parser solverA solverB day example = runSolvers day example parser solverA solverB


runSolvers :: (Show b, Show c) => Int -> ExampleStatus -> (String -> a) -> (a -> b) -> (a -> c) -> IO ()
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


-- Breath first search taken from: 
-- https://topaz.github.io/paste/#XQAAAQBbAwAAAAAAAAAxGYrLUnuO0Fo/becZjJQOKYZXcPSKmNB76DmuoAVlCCI0ZnYTDSCaAJ8mTI5lIVHTSuDlMgAYHgBSWkEJK7dFOK3Kp7XGIDgSSKrpxYX+wdFY9YoqwtNRb6Lkb07qx/u6xWVxBOI5cEXkcJbjoou2F31S0M5YnYxTC7NzD+ecp36C8KLV//KdtK1LoNwrrVU6P+n2Rk8k+JPwfUJQPiOFQBIV78ObhkUXg2XcmCulwx052kjTCREEzikCylpiFya1jUjRNyhg9szMMpn56i/HjMlb6pF80nJiSl+3mLba8kYdtcSe/pmI4N3TwuKtOWMWEO4MC9kkzar9Rp2K2G8Ei+fb9vOS/CzP1cmN8hmqCsVMtueLVXR2CgYs/16CfkAdTKpWnaitBHWHiHwGm1Oo0tngHkvxjJfPu5J1Ek8i0usXSWC7PMGOWr7haPeJZ1syYrQ0UPgPRWlcnS68H/RePGj5BsiPqQFoidMB31jCoCUY1unIFCbDwR/2XnhQ8iZ2KxtK4LX0AX4BWrBVt5zGyQN1vrYK42FnGSWRudJBQqrE9l2Oh/5hiNX/plYcGA==

bfsOnTarget
  :: (Show v, Ord v, Eq v, Ord a, Eq a, Show a)
  => (a -> v) -- repr function for storing search elements in the visited set
  -> (a -> Seq.Seq a) -- step function, get the next states
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
    = let nbrs   = step cur
          queue' = rest Seq.>< nbrs
          seen'  = S.insert (repr cur) seen
      in  cur : help seen' queue'

bfsOn
  :: (Show v, Ord v, Ord a, Eq a, Show a)
  => (a -> v)
  -> (a -> Seq.Seq a)
  -> a
  -> [a]
bfsOn repr step start = bfsOnTarget repr step start (const False)