-- Sudoku Verifier
-- Author: Mikolaj Sinicka
-- Doing this alone, have talked with Jacek, it was ok.
--
-- Instructions to run tests:
-- 1. Place this file in the same folder as the test files (conflicts.txt, blockings.txt, easy50.txt, inconsistent20.txt).
-- 2. Load the file in GHCi:   ghci Sudoku.hs
-- 3. Run:                     main
--
-- Each test returns a list of Bools indicating if each puzzle is consistent (True) or inconsistent (False).
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isDigit" #-}
{-# HLINT ignore "Use catMaybes" #-}

module Sudoku where

-------------------------------------------------------------------------------
-- Types and Constants
-------------------------------------------------------------------------------
type Sudoku = String

type Board = [(String, Int)]

type BoardNumbers = [(String, [Int])]

rows, cols :: String
rows = "ABCDEFGHI"
cols = "123456789"

squares :: [String]
squares = cross rows cols

-------------------------------------------------------------------------------
-- Basic Utilities
-------------------------------------------------------------------------------

replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c == '.' then '0' else c)

cross :: [Char] -> [Char] -> [String]
cross rs cs = [[r, c] | r <- rs, c <- cs]

myDigitToInt :: Char -> Int
myDigitToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | otherwise = error ("Non-digit character: " ++ [c])

parseBoard :: Sudoku -> Board
parseBoard board =
  let zeros = replacePointsWithZeros board
      digits = map myDigitToInt zeros
   in zip squares digits

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

justifyList :: [Maybe a] -> [a]
justifyList xs = [x | Just x <- xs]

lookups :: (Eq a) => [a] -> [(a, b)] -> [b]
lookups xs pairs = justifyList (map (`lookup` pairs) xs)

-------------------------------------------------------------------------------
-- Units and Peers
-------------------------------------------------------------------------------

rowUnits, colUnits :: [[String]]
rowUnits = [cross [r] cols | r <- rows]
colUnits = [cross rows [c] | c <- cols]

boxRows, boxCols :: [String]
boxRows = ["ABC", "DEF", "GHI"]
boxCols = ["123", "456", "789"]

boxUnits :: [[String]]
boxUnits = [[[r, c] | r <- rs, c <- cs] | rs <- boxRows, cs <- boxCols]

unitList :: [[String]]
unitList = rowUnits ++ colUnits ++ boxUnits

filterUnitList :: String -> [[String]]
filterUnitList sq = filter (elem sq) unitList

units :: [(String, [[String]])]
units = [(sq, filterUnitList sq) | sq <- squares]

peers :: [(String, [String])]
peers =
  [ ( sq,
      filter (/= sq) . removeDuplicates . concat $ allUnits
    )
    | (sq, allUnits) <- units
  ]

getPeers :: String -> [String]
getPeers sq = fromMaybe [] (lookup sq peers)

-------------------------------------------------------------------------------
-- 1) Basic Validity Check (no two filled squares in conflict)
-------------------------------------------------------------------------------

validSquare :: (String, Int) -> Board -> Bool
validSquare (square, value) board
  | value == 0 = True
  | otherwise =
      let peerValues = lookups (getPeers square) board
       in value `notElem` peerValues

validBoard :: Board -> Bool
validBoard board = all (`validSquare` board) board

-------------------------------------------------------------------------------
-- 2) Extended Check: Possible Values + Blocking Conflicts
-------------------------------------------------------------------------------

validSquareNumbers :: (String, Int) -> Board -> (String, [Int])
validSquareNumbers (sq, val) board
  | val /= 0 =
      let peerValues = lookups (getPeers sq) board
       in if val `notElem` peerValues then (sq, [val]) else (sq, [])
  | otherwise =
      let peerValues = lookups (getPeers sq) board
          allDigits = [1 .. 9]
          validDigits = [d | d <- allDigits, d `notElem` peerValues]
       in (sq, validDigits)

validBoardNumbers :: Board -> BoardNumbers
validBoardNumbers board =
  map (`validSquareNumbers` board) board

validUnit :: [String] -> BoardNumbers -> Bool
validUnit unit boardNumbers =
  let squaresPoss = lookups unit boardNumbers
      singletons = [v | [v] <- squaresPoss]
      singletonsOk = length singletons == length (removeDuplicates singletons)
      fillAllDigitsOk = all (\d -> any (d `elem`) squaresPoss) [1 .. 9]
   in singletonsOk && fillAllDigitsOk

validUnits :: BoardNumbers -> Bool
validUnits boardNumbers = all (`validUnit` boardNumbers) unitList

-------------------------------------------------------------------------------
-- 3) verifySudoku combining both checks
-------------------------------------------------------------------------------

verifySudoku :: Sudoku -> Bool
verifySudoku sudoku =
  let board = parseBoard sudoku
      boardNumbers = validBoardNumbers board
   in validBoard board && validUnits boardNumbers

-------------------------------------------------------------------------------
-- 4) File parsing and testing
-------------------------------------------------------------------------------

-- Splits file content into list of 81-char puzzles
splitPuzzles :: String -> [Sudoku]
splitPuzzles content =
  let linesOfFile = lines content
      filteredLines = filter (/= "========") linesOfFile
      grouped = groupEvery 9 filteredLines
   in map concat grouped

-- Helper: group a list into sublists of n elements
groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n xs = take n xs : groupEvery n (drop n xs)

verifyFile :: FilePath -> IO [Bool]
verifyFile path = do
  content <- readFile path
  let puzzles = splitPuzzles content
  return (map verifySudoku puzzles)

main :: IO ()
main = do
  results <- verifyFile "inconsistent20.txt"
  mapM_ print results