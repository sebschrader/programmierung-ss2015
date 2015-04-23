-- Uncomment the following line to hide the automatically available Haskell
-- Prelude definitions of the maximum and length functions:
--import Prelude hiding (maximum, length)

-- (a)
l :: [[Int]]
l = [[2,4,6] ,[], [-1]]

-- (b)
max_length :: [[Int]] -> Int
max_length xs = maximum (lengths xs)

-- The Haskell Prelude has already length and maximum defined. Here are
-- a few possible definitions if you want to define them yourselves.
{-
length :: [a] -> Int
length [] = 0
length (x:xs) = length xs + 1

maximum :: [Int] -> Int
maximum []     = error "empty list"
maximum [x]    = x
maximum (x:xs) = let m = maximum xs in if x > m then x else m
-}

lengths :: [[Int]] -> [Int]
lengths []     = []
lengths (l:ls) = length l:lengths ls

-- Advanced: Use map to transform a list of lists to a list of lengths of those
lengths' :: [[Int]] -> [Int]
lengths' = map length

