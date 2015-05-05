-- (a)
incEntry :: [Int] -> Int -> [Int]
-- Optional: Check for negative indexes and abort
incEntry _      n | n < 0 = error "negative index"
-- Idea: If list is too short, use the built-in replicate function to create a
-- list of the remaining missing zeros
incEntry []     n = incEntry (replicate n 0) n
-- if you don't like replicate you can implement it by hand with as follows:
--incEntry []     0 = [1]
--incEntry []     n = 0:incEntry [] (n-1)
incEntry (x:xs) 0 = x+1:xs
incEntry (x:xs) n = x:incEntry xs (n-1)

-- (b)
example = [8, 11, 13, 17]

rsum ::  [Int] -> [Int]
rsum xs = rsum' 0 (reverse xs)
    where
        rsum' n []     = []
        rsum' n (x:xs) = let s = x+n
                         in s:rsum_foo s xs

-- Advanced: Use the built-in scanl function:
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:scanl
-- Note: Although they have the same name, the rsum' from the where in rsum has
-- nothing to do with the rsum' defined below.
rsum' :: [Int] -> [Int]
rsum' l = tail (scan (+) 0 (reverse l))
-- point-free: We can also omit the explicit argument and use function
-- composition:
--rsum' = tail . scanl (+) 0 . reverse

