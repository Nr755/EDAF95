{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}
module SolveSudoku where

import Data.Char (digitToInt, isDigit)
import Data.List (delete, union)
import Data.Maybe (fromMaybe)

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

-------------------------------------------------------------------------------
-- Assign Function
-------------------------------------------------------------------------------

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
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]

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
-- Test
-------------------------------------------------------------------------------

testSudokuString :: String
testSudokuString =
  "53..7...."
    ++ "6..195..."
    ++ ".98....6."
    ++ "8...6...3"
    ++ "4..8.3..1"
    ++ "7...2...6"
    ++ ".6....28."
    ++ "...419..5"
    ++ "....8..79"

testParsedBoard :: Maybe Board
testParsedBoard = parseBoard testSudokuString

printParsedBoard :: Maybe Board -> IO ()
printParsedBoard Nothing = putStrLn "Parsing failed: Invalid Sudoku input"
printParsedBoard (Just board) = mapM_ print board -- Prints each square and its possible values

-- main :: IO ()
-- main = do
--   putStrLn "Parsing Sudoku board..."
--   printParsedBoard testParsedBoard

-------------------------------------------------------------------------------
-- Solve
-------------------------------------------------------------------------------

solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] board = Just board -- Base case: No more squares to process, solution found.
solveSudoku' (s : squares) board =
  firstJust [assign v s board `maybeBind` solveSudoku' squares | v <- lookupList s board]

solveSudoku :: String -> Maybe Board
solveSudoku sudokuString =
  parseBoard sudokuString `maybeBind` solveSudoku' squares

testSudokuString2 :: String
testSudokuString2 =
  "53..7...."
    ++ "6..195..."
    ++ ".98....6."
    ++ "8...6...3"
    ++ "4..8.3..1"
    ++ "7...2...6"
    ++ ".6....28."
    ++ "...419..5"
    ++ "....8..79"

printSolution :: Maybe Board -> IO ()
printSolution Nothing = putStrLn "No solution found!"
printSolution (Just board) = mapM_ print board

printBoardGrid :: Maybe Board -> IO ()
printBoardGrid Nothing = putStrLn "No solution found!"
printBoardGrid (Just board) = do
  putStrLn "Solved Sudoku:"
  mapM_ putStrLn formattedRows
  where
    -- Create a map for easy lookup
    boardMap = board
    getVal sq = case lookup sq boardMap of
      Just [v] -> show v
      _ -> "." -- should not happen in a solved board

    -- Build the rows
    buildRow r = [getVal [r, c] | c <- cols]
    formattedRows = concatMap formatRow ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']

    formatRow r
      | r `elem` ['D', 'G'] = [separator, rowString]
      | otherwise = [rowString]
      where
        rowString = unwords (insertBars $ buildRow r)
        insertBars xs = take 3 xs ++ ["|"] ++ take 3 (drop 3 xs) ++ ["|"] ++ drop 6 xs
        separator = replicate 21 '-'

main :: IO ()
main = do
  putStrLn "Solving Sudoku..."
  printBoardGrid (solveSudoku testSudokuString)
