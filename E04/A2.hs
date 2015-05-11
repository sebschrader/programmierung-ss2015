module E04.A2 where

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

-- Idea: Use a argument to count the distance from the root node.
path :: Tree a -> Tree Int
path = path' 1
-- The first argument has been omitted above:
--path t  = path' 1 t
    where path' :: Int -> Tree a ->  Tree Int
          path' n (Leaf _    ) = Leaf n
          path' n (Node _ l r) = Node n (path' (n+1) l) (path' (n+1) r)

nodes :: Tree a -> Tree [a]
-- Please note: The variable a in the type signature above is not the same as
-- the variable a used in the patterns below. The first refers to a type (e.g.
-- Int), while the latter refers to a value of that type (e.g. 5).
nodes (Leaf a    ) = Leaf [a]
nodes (Node a l r) = Node [a, getKey l, getKey r] (nodes l) (nodes r)
    where getKey :: Tree a -> a
          getKey (Leaf a    ) = a
          getKey (Node a _ _) = a

