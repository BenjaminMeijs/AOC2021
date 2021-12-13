module Day10(solvers) where
import Data.Maybe
import Data.List
import AOC

solvers = createSolvers parseInput solveA solveB 

parseInput  = lines

solveA = sum . mapMaybe (fmap faultScore . findFirstWrong)

findFirstWrong :: String -> Maybe Char
findFirstWrong = goFindFirstWrong []
  where goFindFirstWrong :: [Char] -> String -> Maybe Char
        goFindFirstWrong _ [] = Nothing
        goFindFirstWrong s (x:xs)
          | isOpen x  = fOpen  s (x:xs)
          | otherwise = fClose s (x:xs)
        fOpen  _   []    = Nothing
        fOpen  s  (x:xs) = goFindFirstWrong (matchingClose x:s) xs
        fClose _   []    = Nothing
        fClose [] (x:xs) = error "vraag"
        fClose (s1:ss) (x:xs)
          | x == s1   = goFindFirstWrong ss xs
          | otherwise = Just x

isOpen x = x `elem` ['(','[','<','{']

matchingClose :: Char -> Char
matchingClose '(' = ')'
matchingClose '[' = ']'
matchingClose '<' = '>'
matchingClose '{' = '}'
matchingClose _ = error "This is not an open bracket"

faultScore ')' = 3
faultScore ']' = 57
faultScore '}' = 1197 
faultScore '>' = 25137
faultScore _   = error "This is not a close bracket"

solveB = getMiddle . sort . mapMaybe (fmap completeScore . findWrongStack)

getMiddle xs = xs !! (length xs `div` 2)


completeScore = foldl (\r v -> r*5 + missingValue v) 0

missingValue :: Char -> Int
missingValue ')' = 1
missingValue ']' = 2
missingValue '}' = 3 
missingValue '>' = 4
missingValue _ = error "This is not a close bracket"

findWrongStack :: String -> Maybe [Char]
findWrongStack = goFindWrong []
  where
        goFindWrong s [] = Just s
        goFindWrong s (x:xs)
          | isOpen x  = fOpen  s (x:xs)
          | otherwise = fClose s (x:xs)
        fOpen  _   []    = error "Does not happen"
        fOpen  s  (x:xs) = goFindWrong (matchingClose x:s) xs
        fClose _   []    = error "Does not happen"
        fClose [] (x:xs) = error "Missing on stack"
        fClose (s1:ss) (x:xs)
          | x == s1   = goFindWrong ss xs
          | otherwise = Nothing