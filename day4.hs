import Data.List
import Debug.Trace as T
import Data.Maybe

main = do
    inputText <- readFile "inputs/day4.txt" 
    let input = parseInput inputText
    print $ solveA input
    print $ solveB input

type Card = [[Cell]]
data Cell = Unmarked Int
          | Marked Int
          deriving (Eq, Show)
          
isMarked :: Cell -> Bool
isMarked (Marked _) = True
isMarked _ = False

getCellNum :: Cell -> Int
getCellNum (Marked i) = i
getCellNum (Unmarked i) = i

parseInput :: String -> ([Int], [Card])
parseInput input = (calls,cards)
    where (start:rest) = lines input
          calls = map read $ wordsWhen (==',') start
          cards = map parseCard . filter (/=([""]))$ groupBy (\x y -> x /= "" && y /= "") rest


parseCard :: [String] -> Card
parseCard input = map (map parseCell . words) input

parseCell :: String -> Cell
parseCell input = Unmarked $ read input

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


solveA (calls, cards) = winCall * getCardScore winCard 
  where (winCall, winCard) = getWinningCard calls cards

getCardScore :: Card -> Int
getCardScore card = sum . map getCellNum $ filter (not . isMarked) (concat card)

getWinningCard :: [Int] -> [Card] -> (Int, Card)
getWinningCard calls cards = ( calls !! (length lozers - 1), head $ head winners)
    where cs = reverse $ tails (reverse calls)
          applied = map (filter isBingo) $ map (\c -> map (applyCalls c) cards ) cs
          (lozers, winners) = span (==[]) applied



biMapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
biMapSnd f = map (\(l,r) -> (l, f r) ) 

applyGame :: [Int] -> [Card] -> [(Int, [Card])]
applyGame calls cards = zip calls $ map (\c -> map (applyCalls c) cards ) cs
    where cs = reverse $ tails (reverse calls)


applyCalls :: [Int] -> Card -> Card
applyCalls calls = map (map f)
    where f cell = let n = getCellNum cell in 
                     if n `elem` calls
                     then Marked n
                     else cell

applyCall :: Int -> Card -> Card
applyCall i = applyCalls [i]

isBingo :: Card -> Bool
isBingo card = isHorBingo card || isHorBingo (transpose card)
    where isHorBingo c = any (all isMarked) c 

--This really got very complicated. Next think before you start programming...

solveB (calls, cards) = loseCall * getCardScore loseCard
    where (loseCall, loseCard) = getLosingCard calls cards 

getWinningNumber :: [Int] -> Card -> (Int, Card)
getWinningNumber calls card = fromJust . find (\(_,c) -> isBingo c) $ map (\c -> (head c, applyCalls c card)) callsHeads
    where callsHeads = tail . reverse $ tails (reverse calls)
          

getLosingCard :: [Int] -> [Card] -> (Int, Card)
getLosingCard calls cards = maximumBy f winners
    where winners = map (getWinningNumber calls) cards
          f (callL, _) (callR, _) = compare (g callL) (g callR)
          g x = fromJust $ elemIndex x calls