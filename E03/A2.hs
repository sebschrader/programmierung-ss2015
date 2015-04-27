module E03.A2 where

data Tree = Node Int Tree Tree | NIL deriving (Show)

example = (Node 2 (Node 1 NIL NIL) NIL) :: Tree
-- (a)
insertList :: Tree -> [Int] -> Tree
insertList t [] = t
insertList t (x:xs) =  insertList (insert t x) xs

insert :: Tree -> Int -> Tree
insert NIL y = Node y NIL NIL 
insert (Node x l r) y 
	| y < x = Node x (insert l y) r
	| otherwise = Node x l (insert r y)

insertList' :: Tree -> [Int] -> Tree
insertList'  t xs = foldr (flip insert) t (xs)

-- (b)

treeEqual :: Tree -> Tree -> Bool
treeEqual NIL NIL = True
treeEqual (Node x1 l1 r1) (Node x2 l2 r2) = x1 == x2 && treeEqual l1 l2 && treeEqual r1 r2  
treeEqual _ _ = False

instance Eq Tree where
  (==) = treeEqual

