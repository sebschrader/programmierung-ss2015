module A2 
( pack
, pack'
, pack''
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

--a
---- additional inbuild functions: reverse
-- intuitive idea of having a help function f 
-- ((y:ys):yss) means that we take the first element (y:ys) a [Char] 
-- and the restlist yss a [[Char]] and compare the first element 
-- with our first Element x from our List (x:xs) [Char].
-- But you have to imagine that we reverse it by this step.
-- So we have to reverse it at the end again.
pack :: [Char] -> [[Char]]
pack xs = reverse (f xs [])
    where f :: [Char] -> [[Char]] -> [[Char]]
          f [] ys = ys
          f (x:xs) [] = f xs [[x]]
          f (x:xs) ((y:ys):yss) = if x == y then f xs ((x:y:ys):yss)
                                            else f xs ([x]:(y:ys):yss)
-- additional inbuild functions: foldr
-- previos we did something like foldl
-- to not reverse the list afterwards we now
-- take foldr. It carries the list from right to left and
-- so afterwards its in the right order.
pack' :: [Char] -> [[Char]]
pack' xs = foldr f [] xs
    where f :: Char -> [[Char]] -> [[Char]]
          f x [] = [[x]]
          f x ((y:ys):yss)
              | x == y = ((x:y:ys):yss) 
              | otherwise = ([x]:(y:ys):yss)

-- additional inbuild functions: takeWhile, dropWhile
-- or simply using in build function takeWhile and dropWhile,
-- and using partiell applied function (==)
pack'' :: [Char] -> [[Char]]
pack'' [] = []
pack'' (x:xs) = (takeWhile (==x) (x:xs)) : (pack'' (dropWhile (== x) xs))


-- b
-- additional inbuild functions: map, length 
-- reuse the pack function just to get the lists as elements,
-- after this we just apply a function f to each list element,
-- to transfer it to a tuple.
encode :: [Char] -> [(Int, Char)]
encode xs =  map f (pack xs)
    where f :: [Char] -> (Int, Char)
          f [] = error "This should not happen."
          f (x:xs) = (length (x:xs), x)
          
-- additional inbuild functions: foldr, "anonymous function \", (.)
-- we can leave the pattern blank if they could be applied at 
-- the end of the function
encode' :: [Char] -> [(Int, Char)]
encode' = map (\a -> (length a, head a)) . pack

-- c
-- intuitive idea, to split the first element a tuple 
-- into its pattern (num,lit) and to work than with then
-- with this elements.
decode :: [(Int, Char)] -> [Char]
decode [] = []
decode ((num, lit):xs) = (rep num lit) ++ decode xs
    where rep :: Int -> Char -> [Char]
          rep 0 _ = []
          rep n l = l : rep (n-1) l 
          
-- additional inbuild functions: replicate, fst, snd
-- the same using inbuild functions replicate for make a list of 
-- same elements.
decode' :: [(Int, Char)] -> [Char]
decode' [] = []
decode' (x:xs) =  replicate (fst x) (snd x) ++ decode xs 

-- d
-- additional inbuild functions: head, tail, init, last
-- intuitive idea with ++ to append elements to a list
-- Problem ++ is inefficient
rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xs y 
    | y == 0    = xs
    | y > 0     = rotate ((tail xs) ++ ([head xs])) (y-1)
    | otherwise = rotate (last xs : init xs) (y+1)

-- additional inbuild functions: drop, take, length
-- just use the inbuild function drop and take 
-- to manage a number of elements in a list
-- and than do ++ only once
rotate' :: [Int] -> Int -> [Int]
rotate' xs n 
    | n >= 0 = (drop n xs) ++ (take n xs)
    | otherwise = (drop (length xs + n) xs ) ++ (take (length xs + n) xs)  
