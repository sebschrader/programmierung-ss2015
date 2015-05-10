module E04.A1 where

data Tree = Leaf | Branch Tree Tree deriving (Show, Eq)

-- Solution Using Listcomprehensions.
-- https://wiki.haskell.org/List_comprehension

mkBalanced :: Int -> [Tree]
mkBalanced 0 = []
mkBalanced 1 = [Leaf]
-- all balanced Trees with depth bigger than 1 are a Branch with left and right part
-- first there are tuples of all combinations of depths which are allowed for 
-- left and right childbranch. 
-- Then create for each tuple 2 Childbranches with such depth. 
mkBalanced n = [Branch l r | (hl, hr) <- [(n-2,n-1), (n-1,n-2), (n-1,n-1)]
                            , l <- mkBalanced hl
                            , r <- mkBalanced hr]
