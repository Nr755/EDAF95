{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}
module Lab4 where

map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f, g) (x, y) = (f x, g y)

-- map2 applies two different functions (f and g) to a pair (x, y).

mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf f p = map (\x -> if p x then f x else x)

-- mapIf applies a function f only to elements that satisfy predicate p.

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just x) _ = Just x
maybeOr Nothing y = y

-- maybeOr returns the first Just value if it exists, otherwise it returns the second argument.

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr maybeOr Nothing

-- firstJust finds the first non-Nothing value in a list of Maybe values.

lookupList :: (Eq a) => a -> [(a, [b])] -> [b]
lookupList key xs = maybe [] id (lookup key xs)

-- lookup key xs returns a Maybe [b] (either Just [b] if found, or Nothing).
-- maybe [] id (lookup key xs):
-- If lookup key xs returns Nothing, it uses the default value [].
-- If it returns Just lst, it applies id (which just returns lst as-is).

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x

-- If the first argument is Nothing, the function should return Nothing immediately (indicating failure).
-- Otherwise, if we have Just x, we extract the value x and apply function f to it.
-- Since f itself returns a Maybe b, we stay inside the Maybe context.

tryReplace :: (Eq a) => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x : xs)
  | x == y = Just (y' : xs)
  | otherwise = (x :) <$> tryReplace y y' xs

-- It that attempts to replace the first occurrence of an element in a list with a new element.
-- If the element is found, it returns Just the modified list. If the element is not found, it returns Nothing.

-- Try replacing elements from ys with corresponding zs in xs
recursiveReplacement :: (Eq a) => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement xs [] [] = Just xs -- Base case: No more replacements
recursiveReplacement xs (y : ys) (z : zs) =
  tryReplace y z xs `maybeBind` (\newXs -> recursiveReplacement newXs ys zs)
recursiveReplacement _ _ _ = Nothing -- If the lists are of different lengths, return Nothing

-- Base case: If no replacements are left (ys and zs are empty), return Just xs (the modified list).
-- Recursive case:
-- Apply tryReplace on the first pair (y, z).
-- If successful (Just newXs), recurse with newXs and the remaining replacements.
-- If tryReplace fails, return Nothing.

type Board = [(String, [Int])] -- Each square has a name (e.g., "A1") and a list of possible values.

setValue :: Int -> String -> Board -> Board
setValue value square =
  mapIf (map2 (id, const [value])) (\(s, _) -> s == square)

-- Assign only the given value to the square, overriding any existing possibilities.

eliminateValue :: Int -> String -> Board -> Board
eliminateValue value square =
  mapIf (map2 (id, filter (/= value))) (\(s, vs) -> s == square && value `elem` vs)

--  Remove value from the list of possibilities if it exists.

eliminate :: Int -> String -> Board -> Maybe Board
eliminate value square board
  | square `notElem` map fst board = Just board -- Square not in board, return unchanged
  | value `notElem` lookupList square board = Just board -- Value already eliminated, return unchanged
  | null newValues = Nothing -- No possible values left (unsolvable state)
  | length newValues == 1 = Just (setValue (head newValues) square board) -- Only one value left, fix it
  | otherwise = Just newBoard -- Successfully eliminated the value
  where
    newBoard = eliminateValue value square board -- Attempt elimination
    newValues = lookupList square newBoard -- Check remaining possible values

-- If square is not in the board, return Just board (no changes needed).
-- If value is not in lookupList square board, return Just board (no changes needed).
-- If eliminating the value leaves an empty list (null newValues), return Nothing.
-- If exactly one value remains, fix it using setValue.
-- Otherwise, if successfully eliminated the value, return Just newBoard.