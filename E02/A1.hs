module A1
( compare'
, compare''
, compare'''
, merge
, merge'
) where

-- a
-- intuitive idea with patterns
compare' :: [Int] -> [Int] -> Bool
compare' [] [] = True
compare' [] _  = False
compare' _ []  = False
compare' (x:xs) (y:ys) = (x == y) &&  (compare' xs ys)

-- additional built-in functions: length, foldr, zipWith
compare'' :: [Int] -> [Int] -> Bool
compare'' xs ys =  (length xs) == (length  ys) &&
                    (foldr (&&) True $ zipWith (==) xs ys)

-- additional built-in functions: and, foldr, zipWith, length
-- the foldr (&&) True could also exchanged by and
-- for usage with infint list we have to switch the order of evaluation
compare''' :: [Int] -> [Int] -> Bool
compare''' xs ys =  (and $ zipWith (==) xs ys) && length xs == length  ys

-- (b)
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs     (y:ys)
    | otherwise = y : merge (x:xs) ys

merge' :: [Int] -> [Int] -> [Int]
merge' xs [] = xs
merge' [] ys = ys
-- We can assign a matched subpattern a name with an "as-pattern":
merge' xs'@(x:xs) ys'@(y:ys)
    | x < y     = x : merge xs  ys'
    | otherwise = y : merge xs' ys

