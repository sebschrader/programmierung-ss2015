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

-- additional inbuild functions: length, foldr, zipWith
compare'' :: [Int] -> [Int] -> Bool
compare'' xs ys =  (length xs) == (length  ys) && 
                    (foldr (&&) True $ zipWith (==) xs ys)

-- additional inbuild functions: and, foldr, zipWith, length
-- the foldr (&&) True could also exchanged by and
-- for usage with infint list we have to switch the order of evaluation
compare''' :: [Int] -> [Int] -> Bool
compare''' xs ys =  (and $ zipWith (==) xs ys) && length xs == length  ys 

-- b
-- intuitive idea with pattern matching and guards
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
       | x < y  = x : merge xs (y:ys)
       | x >= y = y : merge (y:ys) xs
       | otherwise = error ("This should not happen! xs:"
                    ++ (show xs) ++ "ys:" ++ (show ys))  

-- additional inbuild functions: min
-- idea of taking to compare allways the first elements of booth lists 
-- and take the smaller one.
merge' :: [Int] -> [Int] -> [Int]
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys) = min x y : merge' xs ys
