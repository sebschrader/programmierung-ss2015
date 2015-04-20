module E01.A2 where
-- Explicit and ugly:
f :: [Int] -> [Int]
f [] = []
f (x:xs) = if x > 0 then f xs ++ [x]
                    else f xs

-- Let's compose it out of two functions:
-- * reverse: reverses a list and is already included in standard Haskell
-- * nonzero: filters all elements of a list greater than 0
f1 ::  [Int] -> [Int]
f1 xs = reverse (nonzero xs)

-- We can omit the parentheses with the $ operator
f2 :: [Int] -> [Int]
f2 xs = reverse $ nonzero xs

-- Or we can use function composition instead and omit the argument
f3 :: [Int] -> [Int]
f3 = reverse . nonzero

nonzero :: [Int] -> [Int]
nonzero [] = []
nonzero (x:xs) | x > 0     = x:nonzero xs
               | otherwise = nonzero xs

-- Advanced: Higher order function filter:
-- Filters a list with a predicate (function p from a to Bool)
{-
filter :: (a -> Bool) -> [a] -> [a]
filter _ []    = []
filter p (x:xs) | pred x    = x : filter p xs
                | otherwise = filter p xs
-}
nonzero1 :: [Int] -> [Int]
nonzero1 = filter (> 0)

