-- (a)
incEntry :: [Int] -> Int -> [Int]
incEntry []     n = incEntry (replicate n 0) n
incEntry (x:xs) 0 = x+1:xs
incEntry (x:xs) n = x:incEntry xs (n-1)

-- (b)
rsum_example = [8, 11, 13, 17]

rsum ::  [Int] -> [Int]
rsum xs = rsum_foo 0 (reverse xs)
    where
        rsum_foo n []     = []
        rsum_foo n (x:xs) = let s = x+n
                            in s:rsum_foo s xs

rsum' :: [Int] -> [Int]
rsum' = tail . scanl (+) 0 . reverse

