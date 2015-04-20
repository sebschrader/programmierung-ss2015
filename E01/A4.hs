module E01.A4 where
gen :: [Int]
gen = genFrom 1

genFrom :: Int -> [Int]
genFrom n = n:genFrom (n+1)

gen' :: [Int]
gen' = [1..]

