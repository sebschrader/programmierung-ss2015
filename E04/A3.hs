module E04.A3
where

-- We revisit the former assignment from exercise 1 and redo it using a few
-- helpful builtin Haskell functions:
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:reverse
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:filter
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
--
-- Higher order function are functions that functions as arguments.
-- For example filter has the type signature (a -> Bool) -> [a] -> [a].
--
-- (>) has type of Ord a => a -> (a -> Bool)
-- If we apply just one argument, e.g. 0 to (>) like this (>0) we get a
-- function with type signature a -> Bool. This is called partial application.
--
-- The dot operator (.) composes two functions:
-- (f . g) x == f (g x) for all x

-- (a)
f :: [Int] -> [Int]
f = reverse . filter (>0)

-- (b)
-- pow is a higher order function, because the second argument is a function
-- a -> a
pow :: Int -> (a -> a) -> a -> a
pow 0 _ x = x
pow n f x = pow (n-1) f (f x)

-- We can also use a fancy fold to do the same:
pow' n f = foldr (.) id (replicate n f)

-- (c)
-- here is the first argument
-- of type (a -> a -> b)
pleat :: (a -> a -> b) -> [a] -> [b]
pleat f []         = []
pleat f [a]        = []
pleat f (a1:a2:as) = f a1 a2 : pleat f (a2:as)

-- Advanced: We can use the higher order function zipWith
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:zipWith
-- zipWith is lazy in its seconds argument, so that we do not have the handle
-- the empty list case specially.
pleat' f as = zipWith f as (tail as)

