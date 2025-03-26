module Main where

-- Import our two Sudoku modules
import Sudoku4 qualified as S4
import Sudoku9 qualified as S9
import System.Environment (getArgs)
import System.IO

-- Splits a file into boards by splitting on lines equal to "========"
splitSudokuFile :: [String] -> [[String]]
splitSudokuFile = filter (not . null) . go []
  where
    go acc [] = [reverse acc]
    go acc ("========" : xs) = reverse acc : go [] xs
    go acc (x : xs) = go (x : acc) xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let allLines = lines contents
          boards = splitSudokuFile allLines -- each board is a list of lines
      putStrLn $ "Found " ++ show (length boards) ++ " boards in file."

      -- Process each board with its board number.
      mapM_
        ( \(i, boardLines) -> do
            putStrLn $ "\nBoard #" ++ show i
            let puzzle = concat boardLines -- puzzle as one continuous string
                boardSize = length boardLines -- number of rows in the puzzle
            if boardSize == 4
              then
                if S4.verifySudoku puzzle
                  then putStrLn "  -> Valid 4x4 puzzle!"
                  else putStrLn "  -> Invalid 4x4 puzzle!"
              else
                if boardSize == 9
                  then
                    if S9.verifySudoku puzzle
                      then putStrLn "  -> Valid 9x9 puzzle!"
                      else putStrLn "  -> Invalid 9x9 puzzle!"
                  else
                    putStrLn $ "  -> Unsupported board size: " ++ show boardSize
        )
        (zip [1 ..] boards)
    _ -> putStrLn "Usage: runhaskell CheckSudoku.hs <filename>"
