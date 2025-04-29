{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isDigit" #-}
{-# HLINT ignore "Use catMaybes" #-}
module Sudoku where

type Sudoku = String

rows, rows9, cols, cols9 :: String
rows = "ABCD"
rows9 = "ABCDEFGHI"
cols = "1234"
cols9 = "123456789"

-------------------------------------------------------------------------------
-- Basic Utilities
-------------------------------------------------------------------------------

-- Replace '.' with '0'
replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c == '.' then '0' else c)

-- Cross two strings to get all pairs
cross :: [Char] -> [Char] -> [String]
cross rs cs = [[r, c] | r <- rs, c <- cs]

-- Four-by-four squares
squares4 :: [String]
squares4 = cross rows cols

-- Convert Char '0'..'9' to Int
myDigitToInt :: Char -> Int
myDigitToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | otherwise = error ("Non-digit character: " ++ [c])

-- Convert Sudoku string to a Board of (squareID, value)
parseBoard :: Sudoku -> [(String, Int)]
parseBoard board =
  let zeros = replacePointsWithZeros board
      digits = map myDigitToInt zeros
   in zip squares4 digits

-- Remove duplicates from a list
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- A custom fromMaybe
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- Convert a list of Maybe values to plain values (drop Nothings)
justifyList :: [Maybe a] -> [a]
justifyList xs = [x | Just x <- xs]

-- Lookup multiple keys in an association list
lookups :: (Eq a) => [a] -> [(a, b)] -> [b]
lookups xs pairs = justifyList (map (`lookup` pairs) xs)

-- For each square, store the list of the 3 units that square belongs to
rowUnits, colUnits :: [[String]]
rowUnits = [cross [r] cols | r <- rows]
colUnits = [cross rows [c] | c <- cols]

boxRows, boxCols :: [String]
boxRows = ["AB", "CD"] -- Two groups of rows
boxCols = ["12", "34"] -- Two groups of cols

boxUnits :: [[String]]
boxUnits =
  [ [[r, c] | r <- rs, c <- cs]
    | rs <- boxRows,
      cs <- boxCols
  ]

unitList :: [[String]]
unitList = rowUnits ++ colUnits ++ boxUnits

filterUnitList :: String -> [[String]]
filterUnitList sq = filter (elem sq) unitList

units :: [(String, [[String]])]
units = [(sq, filterUnitList sq) | sq <- squares4]

-------------------------------------------------------------------------------
-- Peers
-------------------------------------------------------------------------------

-- A square’s peers are all squares in its row, col, and box, excluding itself
peers :: [(String, [String])]
peers =
  [ ( sq,
      filter (/= sq)
        . removeDuplicates
        . concat
        $ allUnits
    )
    | (sq, allUnits) <- units
  ]

getPeers :: String -> [String]
getPeers sq = fromMaybe [] (lookup sq peers)

-------------------------------------------------------------------------------
-- 1) Basic Validity Check (no two filled squares in conflict)
-------------------------------------------------------------------------------

-- Check a single square for conflict with peers
validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (square, value) board
  | value == 0 = True -- empty square => no conflict
  | otherwise =
      let peerValues = lookups (getPeers square) board
       in value `notElem` peerValues

-- Check the entire board for conflicts
validBoard :: [(String, Int)] -> Bool
validBoard board = all (`validSquare` board) board

-------------------------------------------------------------------------------
-- 2) Extended Check: Possible Values + Blocking Conflicts
-------------------------------------------------------------------------------

-- For each square, compute which values are still possible
--  (If filled and valid, the only possibility is that single value;
--   If filled but in conflict, possibilities = [];
--   If empty, it's [1..4] minus any peer-values.)
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, val) board
  | val /= 0 =
      let peerValues = lookups (getPeers sq) board
       in if val `notElem` peerValues
            then (sq, [val])
            else (sq, [])
  | otherwise =
      let peerValues = lookups (getPeers sq) board
          allDigits = [1 .. 4]
          validDigits = [d | d <- allDigits, d `notElem` peerValues]
       in (sq, validDigits)

-- Produce a Board with each square’s possible values
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers board =
  map (`validSquareNumbers` board) board

-- Check a single unit for blocking conflicts
validUnit ::
  [String] -> -- e.g. ["A1","A2","B1","B2"]
  [(String, [Int])] -> -- Board with possible values
  Bool
validUnit unit boardNumbers =
  let squaresPoss = lookups unit boardNumbers

      -- All squares that have exactly 1 possible value
      singletons = [v | [v] <- squaresPoss]

      -- Condition (1): No two singletons share the same value
      singletonsOk =
        length singletons == length (removeDuplicates singletons)

      -- Condition (2): Each digit 1..4 can still appear in at least one square
      fillAllDigitsOk =
        all (\d -> any (d `elem`) squaresPoss) [1 .. 4]
   in singletonsOk && fillAllDigitsOk

-- Check all units for blocking conflicts
validUnits :: [(String, [Int])] -> Bool
validUnits boardNumbers =
  all (`validUnit` boardNumbers) unitList

-------------------------------------------------------------------------------
-- 3) verifySudoku combining both checks
-------------------------------------------------------------------------------

verifySudoku :: Sudoku -> Bool
verifySudoku sudoku =
  let board = parseBoard sudoku -- [(String, Int)]
      boardNumbers = validBoardNumbers board -- [(String, [Int])]
   in validBoard board && validUnits boardNumbers
