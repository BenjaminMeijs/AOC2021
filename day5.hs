module Day5 where
import Data.List

solve = do
    inputText <- readFile "inputs/day5.txt" 
    let input = map parseLineSeg . lines $ inputText
    print $ solveA input
    print $ solveB input


type LineSeg = (Coord, Coord)
type Coord = (Int, Int) 

parseLineSeg :: String -> LineSeg
parseLineSeg input = ((read x1, read y1), (read x2, read y2))
    where [l,_,r]  = words input
          [x1, y1] = wordsWhen (==',') l
          [x2, y2] = wordsWhen (==',') r

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

isHorOrVer :: LineSeg -> Bool
isHorOrVer l = isHor l || isVer l

isVer :: LineSeg -> Bool
isVer ((x1,_),(x2,_)) = x1 == x2

isHor :: LineSeg -> Bool
isHor ((_,y1),(_,y2)) = y1 == y2


solveA input = countOverlapping $ filter isHorOrVer input 

countOverlapping input = length . filter (>= 2) .  map length . group . sort $ getManyCoveredCoords input

getManyCoveredCoords :: [LineSeg] -> [Coord]
getManyCoveredCoords = concatMap getCoveredCoords

getCoveredCoords :: LineSeg -> [Coord]
getCoveredCoords line@((x1,y1),(x2,y2)) = zip (enumerate x1 x2) (enumerate y1 y2)
  where enumerate start end
            | start == end = repeat start -- Used for Horizontal and diagonal case
            | start <= end = [start .. end]
            | otherwise = reverse [end .. start]



solveB = countOverlapping