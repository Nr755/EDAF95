module Main where

import Sudoku

main :: IO ()
main = do
  let goodSudoku = "1234341243212143"
  let badSudoku = "1134341243212143"

  putStrLn $ "Verifying valid Sudoku:   " ++ show (verifySudoku goodSudoku)
  putStrLn $ "Verifying invalid Sudoku: " ++ show (verifySudoku badSudoku)
