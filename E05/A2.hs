module E05.A2 where

-- foldl is already defined in the Prelude and is therefore automatically
-- imported into out scope. We have to explicitly hide it.
import Prelude hiding (foldl)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

--   foldl (\x y -> 2*x+y) 0 [1,0,1]
-- = foldl (\x y -> 2*x+y) ((\x y -> 2*x+y) 0 1) [0,1]
-- = foldl (\x y -> 2*x+y) (2*0+1) [0,1]
-- = foldl (\x y -> 2*x+y) 1 [0,1]
-- = foldl (\x y -> 2*x+y) ((\x y -> 2*x+y) 1 0) [1]
-- = foldl (\x y -> 2*x+y) 2 [1]
-- = foldl (\x y -> 2*x+y) ((\x y -> 2*x+y) 2 1) []
-- = foldl (\x y -> 2*x+y) 5 []
-- = 5

-- => This code converts the binary number 101 into its decimal representation

