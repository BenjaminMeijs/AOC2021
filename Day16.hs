module Day16(solvers) where
import Text.ParserCombinators.ReadP as P
import AOC
import Distribution.Simple.Glob (GlobSyntaxError(LiteralFileNameGlobStar))
import Control.Exception.Base (evaluate)

solvers = createSolvers parseInput solveA solveB 16


-- TYPE DEFINITIONS + fold over Packet
data Packet = Packet Version Value deriving (Show, Eq, Ord)
data Value = LiteralValue Int | Operator OperatorType [Packet] deriving (Show, Eq, Ord)
type Version = Int
type OperatorType = Int
type PacketAlgebra v p = (Version -> v -> p, Int -> v, OperatorType -> [p] -> v)
foldPacket :: PacketAlgebra v p -> Packet -> p
foldPacket alg@(fPacket, fLitVal, fOp) (Packet version value) = fPacket version (fValue value)
  where fValue (LiteralValue i)     = fLitVal i
        fValue (Operator opType ps) = fOp opType (map (foldPacket alg) ps)

-- PARSING
parseInput :: String -> Packet
parseInput = parseWith pInput . concatMap hexToBin

pInput :: ReadP Packet
pInput = pPacket <* many (char '0')

pPacket :: ReadP Packet
pPacket = do 
  version <- pVersion 
  typeId <- pTypeId
  value <- pValue typeId
  return $ Packet version value

pVersion :: ReadP Version
pVersion = pBinInt 3
pTypeId :: ReadP OperatorType
pTypeId  = pBinInt 3

pValue :: Int -> ReadP Value
pValue typeID = case typeID of
    4 -> pLiteralValue
    i -> pOperator i

pLiteralValue :: ReadP Value
pLiteralValue = (\x y -> LiteralValue ( binToInt $concat x++y)) <$> many pNotLastGroup <*> pLastGroup
  where pNotLastGroup = char '1' *> count 4 get
        pLastGroup    = char '0' *> count 4 get

pOperator :: Int -> ReadP Value
pOperator opCode = (char '0' *> p15Operator) +++ (char '1' *> p11Operator)
  where p15Operator = do 
          size <- pBinInt 15
          substring <- count size get
          let subpackets = parseWith (many pPacket) substring
          return $ Operator opCode subpackets 
        p11Operator = do 
          size <- pBinInt 11
          subpackets <- count size pPacket
          return $ Operator opCode subpackets

pBinInt :: Int -> ReadP Int
pBinInt size = binToInt <$> count size get

-- PART A
solveA :: Packet -> Int
solveA = sumVersion

sumVersion :: Packet -> Int 
sumVersion = foldPacket ((+), const 0, const sum) 


-- PART B
solveB :: Packet -> Int
solveB = evalPacket

evalPacket :: Packet -> Int
evalPacket = foldPacket (const id, id, evalOp)

evalOp :: OperatorType -> [Int] -> Int
evalOp 0 xs = sum xs
evalOp 1 xs = product xs
evalOp 2 xs = minimum xs
evalOp 3 xs = maximum xs
evalOp 5 [l,r] = boolToInt $ l > r 
evalOp 6 [l,r] = boolToInt $ l < r
evalOp 7 [l,r] = boolToInt $ l == r
evalOp _ _ = error "Unknown opcode"


-- Helper fucntions
boolToInt :: Num p => Bool -> p
boolToInt True  = 1
boolToInt False = 0

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin  _  = error "HexToBin expects a char that is hexadecimal"

binToInt :: String -> Int
binToInt [] = 0
binToInt xs = fst $ foldr f (0,1) xs 
  where f c (sum, pow) = case c of
            '1' -> (sum + pow, pow*2)
            '0' -> (sum      , pow*2)
            _ -> error "bin To Int expects a string containing only 1's and 0's"