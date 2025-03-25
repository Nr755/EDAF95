module Sudoku where

import Data.Char

rows = "ABCD"

rows9 = "ABCDEFGHI"

cols = "1234"

cols9 = "123456789"

containsElem :: (Eq a) => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x : xs)
  | elem == x = True
  | otherwise = containsElem elem xs

-- cross :: [a] -> [a] -> [[a]]
-- cross xs ys = [[x, y] | x <- xs, y <- ys]

replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c == '.' then '0' else c)

cross :: [Char] -> [Char] -> [String]
cross rs cs = [[r, c] | r <- rs, c <- cs]

squares4 :: [String]
squares4 = cross rows cols

parseBoard :: String -> [(String, Int)]
parseBoard board =
  let zeros = replacePointsWithZeros board
      digits = map digitToInt zeros
   in zip squares4 digits

rowUnits, colUnits :: [[String]]
rowUnits = [cross [r] cols | r <- rows]
colUnits = [cross rows [c] | c <- cols]

boxRows, boxCols :: [String]
boxRows = ["AB", "CD"] -- two groups of row letters
boxCols = ["12", "34"] -- two groups of column digits

boxUnits :: [[String]]
boxUnits =
  [ [[r, c] | r <- rs, c <- cs] -- produce squares like "A1","A2","B1","B2"
    | rs <- boxRows,
      cs <- boxCols
  ]

unitList :: [[String]]
unitList = rowUnits ++ colUnits ++ boxUnits

filterUnitList :: String -> [[String]]
filterUnitList sq = filter (containsElem sq) unitList

units :: [(String, [[String]])]
units = [(sq, filterUnitList sq) | sq <- squares4]

foldList :: [[a]] -> [a]
foldList = concat

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) =
  x : removeDuplicates (filter (/= x) xs)

peers :: [(String, [String])]
peers =
  [ ( sq,
      filter (/= sq) -- remove the square
        . removeDuplicates -- remove duplicates
        . concat -- flatten the 3 units
        $ allUnits
    )
    | (sq, allUnits) <- units
  ]
