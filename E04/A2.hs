module E04.A2 
( Tree
, path
, nodes
)
where

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

path :: Tree a -> Tree Int
path t  = path' 1 t
    where path' :: Int -> Tree a ->  Tree Int
          path' x (Leaf a) = (Leaf x)
          path' x (Node a t1 t2) = (Node x (path' (x+1) t1) (path' (x+1) t2)) 

nodes :: Tree a -> Tree [a]
nodes (Leaf x) = (Leaf [x])
nodes (Node x t1 t2) = (Node  [x,(getE t1),(getE t2)] (nodes t1) (nodes t2))
    where getE :: Tree a -> a
          getE (Leaf x) = x
          getE (Node x _ _) = x