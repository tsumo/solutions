module CollatzConjecture (collatz) where

rec :: Integer -> Integer -> Maybe Integer
rec x acc
    | x <= 0 = Nothing
    | x == 1 = Just acc
    | even x = rec (x `div` 2) (acc + 1)
    | odd x = rec (3 * x + 1) (acc + 1)
    | otherwise = Nothing

collatz :: Integer -> Maybe Integer
collatz x = rec x 0

