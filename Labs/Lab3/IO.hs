module IO where

import System.Random (randomRIO)

giveMeANumber :: IO ()
giveMeANumber = do
  putStrLn "Enter the lower bound:"
  lowerStr <- getLine
  putStrLn "Enter the upper bound:"
  upperStr <- getLine

  -- Parse input as Int
  let lower = read lowerStr :: Int
      upper = read upperStr :: Int

  -- Generate a random Int between lower and upper (inclusive)
  randomNum <- randomRIO (lower, upper)

  putStrLn $ "Your random number is: " ++ show randomNum

printSudoku :: [(String, Int)] -> IO ()
printSudoku board = do
  -- Optional column header
  putStrLn "    1 2 3 4"
  -- For each row A..D
  mapM_
    ( \r -> do
        -- Gather the four squares of that row, e.g. A1,A2,A3,A4
        let rowValues = [lookup [r, c] board | c <- "1234"]
            -- Convert Maybe Int to either "." or the digit as a string
            showCell mb = case mb of
              Just 0 -> "."
              Just n -> show n
              Nothing -> "." -- if the board doesn't list a square at all
            rowString = unwords (map showCell rowValues)
        putStrLn (r : " | " ++ rowString)
    )
    "ABCD"

main :: IO ()
main = do
  let testBoard =
        [ ("A1", 1),
          ("A2", 2),
          ("A3", 3),
          ("A4", 4),
          ("B1", 2),
          ("B2", 0),
          ("B3", 0),
          ("B4", 4),
          ("C1", 0),
          ("C2", 0),
          ("C3", 3),
          ("C4", 1),
          ("D1", 4),
          ("D2", 3),
          ("D3", 0),
          ("D4", 0)
        ]
  putStrLn "Printing test Sudoku board:"
  printSudoku testBoard
