module E04.A4
( Tree
, inorder
, mapTree
  )
where

data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving Show

inorder :: Tree a -> [a]
inorder (Leaf x) =  [x]
inorder (Branch x t1 t2) = (inorder t1) ++ [x] ++ (inorder t2)

-- a function which maps Tree of type a to a Tree of type b 
-- with any function which is a function from a -> b
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)