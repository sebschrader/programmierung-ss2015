module E04.A1 where

data Tree = Leaf | Branch Tree Tree deriving Show

-- A solution using list comprehensions:
-- https://wiki.haskell.org/List_comprehension

mkBalanced :: Int -> [Tree]
mkBalanced n | n < 1 = []
mkBalanced 1         = [Leaf]
-- All balanced Trees with depth bigger than 1 are a Branch with a left and
-- right child. The children should either be equal in height or one may be
-- one level taller than the other, which yields three different height
-- combinations.
-- For each each height-pair, we create all possible child trees using
-- recursion.
mkBalanced n         = [Branch l r | (hl, hr) <- [(n-2,n-1), (n-1,n-2),
                                                  (n-1,n-1)]
                                   , l <- mkBalanced hl
                                   , r <- mkBalanced hr]

