-- Sudoku Solver
-- Author: Mikolaj Sinicka
-- Doing this alone, have talked with Jacek, it was ok.
--
-- Instructions to run:
-- 1. Place this file in the same folder as the test files (easy50.txt, or your own test files).
-- 2. Load the file in GHCi:   ghci SolveSudoku.hs
-- 3. Run:                     main
--
-- Program description:
-- - The program asks for the filename containing Sudoku puzzles (one per line, 81 characters each).
-- - For each Sudoku, the user can:
--    * Press 's' to solve the current Sudoku and show the solution.
--    * Press 'a' to solve all remaining Sudokus automatically.
--    * Press 'i' to insert a value manually (specify square and value).
-- - Pressing any other key will print an error message and exit.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}
module SolveSudoku where

import Data.Char (digitToInt, isDigit, toUpper)
import Data.List (delete, union)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)

-------------------------------------------------------------------------------
-- Board Representation
-------------------------------------------------------------------------------

type Board = [(String, [Int])]

rows, cols :: String
rows = "ABCDEFGHI"
cols = "123456789"

squares :: [String]
squares = [[r, c] | r <- rows, c <- cols]

unitlist :: [[String]]
unitlist =
  [[r : [c] | c <- cols] | r <- rows]
    ++ [[r : [c] | r <- rows] | c <- cols]
    ++ [[[r, c] | r <- rs, c <- cs] | rs <- ["ABC", "DEF", "GHI"], cs <- ["123", "456", "789"]]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

peers :: [(String, [String])]
peers = [(s, delete s (foldl union [] u)) | (s, u) <- units]

getPeers :: String -> [String]
getPeers s = fromMaybe [] (lookup s peers)

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f, g) (x, y) = (f x, g y)

mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf f p = map (\x -> if p x then f x else x)

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just x) _ = Just x
maybeOr Nothing y = y

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr maybeOr Nothing

lookupList :: (Eq a) => a -> [(a, [b])] -> [b]
lookupList key xs = maybe [] id (lookup key xs)

-------------------------------------------------------------------------------
-- Board Manipulation Functions
-------------------------------------------------------------------------------

setValue :: Int -> String -> Board -> Board
setValue value square =
  mapIf (map2 (id, const [value])) (\(s, _) -> s == square)

eliminateValue :: Int -> String -> Board -> Board
eliminateValue value square =
  mapIf (map2 (id, filter (/= value))) (\(s, vs) -> s == square && value `elem` vs)

eliminate :: Int -> String -> Board -> Maybe Board
eliminate value square board
  | square `notElem` map fst board = Just board
  | value `notElem` lookupList square board = Just board
  | null newValues = Nothing
  | length newValues == 1 = Just (setValue (head newValues) square board)
  | otherwise = Just newBoard
  where
    newBoard = eliminateValue value square board
    newValues = lookupList square newBoard

assign :: Int -> String -> Board -> Maybe Board
assign value square board =
  Just (setValue value square board)
    `maybeBind` assign' value (getPeers square)

assign' :: Int -> [String] -> Board -> Maybe Board
assign' _ [] board = Just board
assign' value (p : ps) board =
  eliminate value p board `maybeBind` assign' value ps

-------------------------------------------------------------------------------
-- Parsing Functions
-------------------------------------------------------------------------------

allDigits :: [Int]
allDigits = [1 .. 9]

emptyBoard :: Board
emptyBoard = map (,allDigits) squares

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "Invalid Sudoku input"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

-------------------------------------------------------------------------------
-- Solve Functions
-------------------------------------------------------------------------------

solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] board = Just board
solveSudoku' (s : ss) board =
  firstJust [assign v s board `maybeBind` solveSudoku' ss | v <- lookupList s board]

solveSudoku :: String -> Maybe Board
solveSudoku sudokuString =
  parseBoard sudokuString `maybeBind` solveSudoku' squares

-------------------------------------------------------------------------------
-- Printing Functions
-------------------------------------------------------------------------------

printParsedBoard :: Maybe Board -> IO ()
printParsedBoard Nothing = putStrLn "Parsing failed: Invalid Sudoku input"
printParsedBoard (Just board) = printBoardGrid (Just board)

printSolution :: Maybe Board -> IO ()
printSolution Nothing = putStrLn "No solution found!"
printSolution (Just board) = printBoardGrid (Just board)

printBoardGrid :: Maybe Board -> IO ()
printBoardGrid Nothing = putStrLn "No board to print!"
printBoardGrid (Just board) = do
  mapM_ putStrLn formattedRows
  where
    boardMap = board
    getVal sq = case lookup sq boardMap of
      Just [v] -> show v
      _ -> "."
    buildRow r = [getVal [r, c] | c <- cols]
    formattedRows = concatMap formatRow rows

    formatRow r
      | r `elem` "DG" = [separator, rowString]
      | otherwise = [rowString]
      where
        rowString = unwords (insertBars (buildRow r))
        insertBars xs = take 3 xs ++ ["|"] ++ take 3 (drop 3 xs) ++ ["|"] ++ drop 6 xs
        separator = replicate 21 '-'

-------------------------------------------------------------------------------
-- Reading Sudokus from a file
-------------------------------------------------------------------------------

readSudokusFromFile :: IO [String]
readSudokusFromFile = do
  putStr "Enter filename: "
  hFlush stdout
  filename <- getLine
  contents <- readFile filename
  return (lines contents)

-------------------------------------------------------------------------------
-- Main Menu and REPL
-------------------------------------------------------------------------------

menuLoop :: [(String, Maybe Board)] -> IO ()
menuLoop [] = putStrLn "No more Sudokus to solve!"
menuLoop ((rawStr, parsedBoard) : rest) = do
  putStrLn "\nCurrent Sudoku:"
  printParsedBoard parsedBoard
  putStr "Choose action (s = solve, a = solve all, i = insert): "
  hFlush stdout
  cmd <- getLine
  case map toUpper cmd of
    "S" -> do
      putStrLn "\nSolving current Sudoku..."
      printSolution (solveSudoku rawStr)
      menuLoop rest
    "A" -> do
      putStrLn "\nSolving all remaining Sudokus..."
      solveAll ((rawStr, parsedBoard) : rest)
    "I" -> do
      updated <- insertValue rawStr parsedBoard
      menuLoop ((rawStr, updated) : rest)
    _ -> do
      putStrLn "Invalid input. Exiting."
      return ()

solveAll :: [(String, Maybe Board)] -> IO ()
solveAll [] = return ()
solveAll ((rawStr, parsedBoard) : rest) = do
  putStrLn "\nOriginal Sudoku:"
  printParsedBoard parsedBoard
  putStrLn "\nSolution:"
  printSolution (solveSudoku rawStr)
  solveAll rest

insertValue :: String -> Maybe Board -> IO (Maybe Board)
insertValue _ Nothing = do
  putStrLn "Cannot insert value into invalid board."
  return Nothing
insertValue rawStr (Just board) = do
  putStr "Enter square (e.g., A1): "
  hFlush stdout
  sq <- getLine
  putStr "Enter value (1-9): "
  hFlush stdout
  vStr <- getLine
  let sq' = map toUpper sq
      v = if length vStr == 1 && isDigit (head vStr) then digitToInt (head vStr) else 0
  if sq' `elem` squares && v >= 1 && v <= 9
    then do
      let newBoard = assign v sq' board
      case newBoard of
        Nothing -> do
          putStrLn "Invalid move (conflict)."
          return (Just board)
        Just b -> return (Just b)
    else do
      putStrLn "Invalid square or value."
      return (Just board)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  sudokuStrings <- readSudokusFromFile
  let parsed = map (\str -> (str, parseBoard str)) sudokuStrings
  menuLoop parsed
