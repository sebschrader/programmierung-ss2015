module E02.A2
( pack
, pack'
, encode
, encode'
, decode
, decode'
, rotate
, rotate'
, example1
, example2
, example3
) where

example1 :: [Char]
example1 = ['a','a','b','b','b','a']

example2 :: [(Int,Char)]
example2 = [(2, 'a'), (3, 'b'), (1, 'a')]

example3 :: [Int]
example3 = [1,2,3,4]

-- (a)
-- This problem is somewhat difficult to solve as we have to apply a concept
-- that we never used before: multiple result values and modifying result
-- values from a recursive function application.
-- We need a helper function takeEqual that splits the longest prefix of
-- recurring characters off of a list and gives us both the longest prefix and
-- the remaining list.
-- Using where we can assign through pattern matching both parts of the result
-- of the helper function to variables and use them for the actual result of
-- the result function.
pack :: [Char] -> [[Char]]
pack ""         = []
pack xs'@(x:xs) = ys:pack zs
    where (ys, zs) = takeEqual x xs'

takeEqual :: Char -> [Char] -> ([Char], [Char])
takeEqual _ ""     = ("","")
takeEqual c xs'@(x:xs)
    -- We can do something with let as we have done above with where.
    -- See https://wiki.haskell.org/Let_vs._Where for a discussion of both of
    -- these constructs.
    | x == c    = let (ys, zs) = takeEqual c xs
                  in (x:ys, zs)
    | otherwise = ("", xs')

-- Advanced: Very elegant solution using takeWhile and dropWhile
-- takeWhile: take elements from a list as long as a predicate is satisfied
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:takeWhile
-- dropWhile: drop elements from a list as long as a predicate is satisfied
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:dropWhile
-- We partially apply the equality function (==)
pack' :: [Char] -> [[Char]]
pack' [] = []
pack' xs'@(x:xs) = takeWhile (==x) xs':pack' (dropWhile (== x) xs)


-- (b)
-- additional built-in functions: map, length
-- reuse the pack function just to get the lists as elements,
-- after this we just apply a function f to each list element,
-- to transfer it to a tuple.
encode :: [Char] -> [(Int, Char)]
encode xs =  map f (pack xs)
    where f :: [Char] -> (Int, Char)
          f xs = (length xs, head xs)

-- Advanced: Anonymous function (lambda)
-- The function f is really small, so we can use an anonymous function.
-- We can omit the argument of encode' and use function composition. This is
-- called pointfree style:
-- https://wiki.haskell.org/Pointfree
encode' :: [Char] -> [(Int, Char)]
encode' = map (\l -> (length l, head l)) . pack

-- (c)
-- intuitive idea, to split the first element a tuple
-- into its pattern (num,lit) and to work than with then
-- with this elements.
decode :: [(Int, Char)] -> [Char]
decode [] = ""
decode ((n, c):xs) = rep n c ++ decode xs
    where rep :: Int -> Char -> [Char]
          rep 0 _ = []
          rep n c = c : rep (n-1) c

-- Advanced: Use built-in functions
-- The Haskell prelude already includes the functions replicate, fst, snd
-- replicate: Repeat an element a given number of times
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:replicate
-- fst, snd: Extract components out of pairs
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:fst
decode' :: [(Int, Char)] -> [Char]
decode' [] = []
decode' (x:xs) =  replicate (fst x) (snd x) ++ decode xs

-- (d)
-- additional built-in functions: head, tail, init, last
-- intuitive approach: Shift elements to the end of the list with ++ until
-- the count reaches zero.
-- Problem: ++ can be inefficient
rotate :: [Int] -> Int -> [Int]
rotate []         n = []
rotate xs'@(x:xs) n
    | n == 0    = xs'
    | n < 0     = rotate xs' (length xs' + n)
    | otherwise = rotate (xs ++ [x]) (n-1)

-- Advanced: take, drop, cycle
-- Use the built-in functions take, drop and cycle
-- take: take the first n elements of a list
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:take
-- drop: drop the first n elements of a list
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:drop
-- repeat a list
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:cycle
rotate' :: [Int] -> Int -> [Int]
rotate' xs n
    | n < 0     = rotate' xs (l + n)
    | otherwise = take l $ drop n $ cycle xs
    where l = length xs

