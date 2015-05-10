module E04.A3 
( pow
, pleat
)
where

f :: [Int] -> [Int]
f xs = reverse (filter (>0) xs)

pow :: Int -> (a -> a) -> a -> a
pow 0 _ x = x
pow n f x = pow (n-1) f (f x)

pleat :: (a -> a -> b) -> [a] -> [b]
pleat f [] = []
pleat f [x] = []
pleat f (x:y:xs) = (f x y) : (pleat f (y:xs))