{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isDigit" #-}
{-# HLINT ignore "Use catMaybes" #-}
module Sudoku9 where

type Sudoku = String

-- For a 9×9 Sudoku:
rows9 :: String
rows9 = "ABCDEFGHI"

cols9 :: String
cols9 = "123456789"

-------------------------------------------------------------------------------
-- Basic Utilities
-------------------------------------------------------------------------------

-- Replace '.' with '0'
replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c == '.' then '0' else c)

-- Cross two strings to get all pairs, e.g., "ABC" x "123" = ["A1","A2","A3","B1",...]
cross :: [Char] -> [Char] -> [String]
cross rs cs = [[r, c] | r <- rs, c <- cs]

-- Nine-by-nine squares
squares9 :: [String]
squares9 = cross rows9 cols9

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
   in zip squares9 digits

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

-- Row units: each row of 9 squares
rowUnits :: [[String]]
rowUnits = [cross [r] cols9 | r <- rows9]

-- Column units: each column of 9 squares
colUnits :: [[String]]
colUnits = [cross rows9 [c] | c <- cols9]

-- For 9×9, boxes are 3×3.
boxRows :: [String]
boxRows = ["ABC", "DEF", "GHI"]

boxCols :: [String]
boxCols = ["123", "456", "789"]

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
units = [(sq, filterUnitList sq) | sq <- squares9]

-------------------------------------------------------------------------------
-- Peers
-------------------------------------------------------------------------------

-- A square’s peers are all squares in its row, column, and box, excluding itself
peers :: [(String, [String])]
peers =
  [ ( sq,
      filter (/= sq) -- remove the square itself
        . removeDuplicates -- remove duplicates
        . concat -- flatten the 3 units
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

-- For each square, compute which values are still possible.
-- If filled and valid, the only possibility is that single value;
-- if filled but in conflict, possibilities = [];
-- if empty, it's [1..9] minus any peer-values.
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, val) board
  | val /= 0 =
      let peerValues = lookups (getPeers sq) board
       in if val `notElem` peerValues
            then (sq, [val])
            else (sq, [])
  | otherwise =
      let peerValues = lookups (getPeers sq) board
          allDigits = [1 .. 9]
          validDigits = [d | d <- allDigits, d `notElem` peerValues]
       in (sq, validDigits)

-- Produce a board with each square’s possible values.
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers board = map (`validSquareNumbers` board) board

-- Check a single unit for blocking conflicts
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit boardNumbers =
  let squaresPoss = lookups unit boardNumbers
      -- All squares that have exactly 1 possible value
      singletons = [v | [v] <- squaresPoss]
      -- Condition (1): No two singletons share the same value.
      singletonsOk = length singletons == length (removeDuplicates singletons)
      -- Condition (2): Each digit 1..9 can still appear in at least one square.
      fillAllDigitsOk = all (\d -> any (d `elem`) squaresPoss) [1 .. 9]
   in singletonsOk && fillAllDigitsOk

-- Check all units for blocking conflicts.
validUnits :: [(String, [Int])] -> Bool
validUnits boardNumbers = all (`validUnit` boardNumbers) unitList

-------------------------------------------------------------------------------
-- 3) verifySudoku combining both checks
-------------------------------------------------------------------------------

verifySudoku :: Sudoku -> Bool
verifySudoku sudoku =
  let board = parseBoard sudoku -- [(String, Int)]
      boardNumbers = validBoardNumbers board -- [(String, [Int])]
   in validBoard board && validUnits boardNumbers

-------------------------------------------------------------------------------
-- 4) Printing the Sudoku board
-------------------------------------------------------------------------------

printSudoku :: [(String, Int)] -> IO ()
printSudoku board = do
  let boardNumbers = validBoardNumbers board
  putStrLn "    1 2 3 4 5 6 7 8 9"
  mapM_
    ( \r -> do
        let rowCells =
              [ let cellId = [r, c]
                    valStr = case lookup cellId board of
                      Just 0 -> "."
                      Just n -> show n
                      Nothing -> "."
                    errorMarker = case lookup cellId boardNumbers of
                      Just poss -> if null poss then "!" else ""
                      Nothing -> ""
                 in valStr ++ errorMarker
                | c <- cols9
              ]
        putStrLn (r : " | " ++ unwords rowCells)
    )
    rows9

-------------------------------------------------------------------------------
-- Main function for testing
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- A sample valid 9×9 Sudoku board (solved puzzle)
  let validBoard =
        [ ("A1", 5),
          ("A2", 3),
          ("A3", 4),
          ("A4", 6),
          ("A5", 7),
          ("A6", 8),
          ("A7", 9),
          ("A8", 1),
          ("A9", 2),
          ("B1", 6),
          ("B2", 7),
          ("B3", 2),
          ("B4", 1),
          ("B5", 9),
          ("B6", 5),
          ("B7", 3),
          ("B8", 4),
          ("B9", 8),
          ("C1", 1),
          ("C2", 9),
          ("C3", 8),
          ("C4", 3),
          ("C5", 4),
          ("C6", 2),
          ("C7", 5),
          ("C8", 6),
          ("C9", 7),
          ("D1", 8),
          ("D2", 5),
          ("D3", 9),
          ("D4", 7),
          ("D5", 6),
          ("D6", 1),
          ("D7", 4),
          ("D8", 2),
          ("D9", 3),
          ("E1", 4),
          ("E2", 2),
          ("E3", 6),
          ("E4", 8),
          ("E5", 5),
          ("E6", 3),
          ("E7", 7),
          ("E8", 9),
          ("E9", 1),
          ("F1", 7),
          ("F2", 1),
          ("F3", 3),
          ("F4", 9),
          ("F5", 2),
          ("F6", 4),
          ("F7", 8),
          ("F8", 5),
          ("F9", 6),
          ("G1", 9),
          ("G2", 6),
          ("G3", 1),
          ("G4", 5),
          ("G5", 3),
          ("G6", 7),
          ("G7", 2),
          ("G8", 8),
          ("G9", 4),
          ("H1", 2),
          ("H2", 8),
          ("H3", 7),
          ("H4", 4),
          ("H5", 1),
          ("H6", 9),
          ("H7", 6),
          ("H8", 3),
          ("H9", 5),
          ("I1", 3),
          ("I2", 4),
          ("I3", 5),
          ("I4", 2),
          ("I5", 8),
          ("I6", 6),
          ("I7", 1),
          ("I8", 7),
          ("I9", 9)
        ]
  putStrLn "Printing valid 9×9 Sudoku board:"
  printSudoku validBoard
