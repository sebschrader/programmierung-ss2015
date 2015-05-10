module E04.A3 
( f
, pow
, pleat
)
where

-- using builtin functions von haskell
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:reverse
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:filter
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
-- higherorder function means using functions as argument for functions
-- So filter has type of ((a -> Bool) -> [a] -> [a]) 
-- (>) has type of Ord a => a -> (a -> Bool) 
-- 0 has been partitial applied to (>) . so (>0) has type (a -> Bool)
f :: [Int] -> [Int]
f = reverse . filter (>0)

-- pow is a higher order function, 
-- because the second argument has 
-- type of a function (a -> a)
pow :: Int -> (a -> a) -> a -> a
pow 0 _ x = x
pow n f x = pow (n-1) f (f x)

-- here is the first argument
-- of type (a -> a -> b)
pleat :: (a -> a -> b) -> [a] -> [b]
pleat f [] = []
pleat f [x] = []
pleat f (x:y:xs) = (f x y) : (pleat f (y:xs))