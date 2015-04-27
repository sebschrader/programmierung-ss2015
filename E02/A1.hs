-- We can declare the names a module exports
module A1
( cmpr
, cmpr'
, cmpr''
, merge
, merge'
) where

-- (a)
-- intuitive approach with explicit patterns
cmpr :: [Int] -> [Int] -> Bool
cmpr []     []     = True
cmpr []     _      = False
cmpr _      []     = False
cmpr (x:xs) (y:ys) = (x == y) && (cmpr' xs ys)


-- Advanced: foldr and zipWith
-- zipWith: Combines two lists with an helper function to a new list
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:zipWith
-- foldr: Folds (reduces) a list to a single value using a helper function
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:foldr
-- zipWith truncates the longer list, therefore we have to compare the
-- lengths of the lists too
cmpr' :: [Int] -> [Int] -> Bool
cmpr' xs ys = (length xs) == (length  ys) &&
                 (foldr (&&) True $ zipWith (==) xs ys)

-- Still advanced:
-- foldr (&&) True is a bit cryptic and can be replaced with and
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:and
--
-- length does not work with infinite list, so we can switch the order of
-- evaluation, but then again it can't ever work for equal infinite lists.
cmpr'' :: [Int] -> [Int] -> Bool
cmpr'' xs ys = (and $ zipWith (==) xs ys) && length xs == length  ys

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

