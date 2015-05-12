module E03.A2 where

data Tree = Node Int Tree Tree | NIL deriving (Show)

example = (Node 2 (Node 1 NIL NIL) NIL) :: Tree

-- (a)
insert :: Tree -> [Int] -> Tree
insert t [] = t
insert t (x:xs) =  insert (insertElem t x) xs

insertElem :: Tree -> Int -> Tree
insertElem NIL y    = Node y NIL NIL
insertElem (Node x l r) y
    | y < x     = Node x (insertElem l y) r
    | otherwise = Node x l                (insertElem r y)

-- Advanced: Use foldl to fold the list into a tree
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:foldl
insert' :: Tree -> [Int] -> Tree
insert' t xs = foldl insertElem t xs
-- point-free:
--insert' = foldl insert

-- (b)

treeEqual :: Tree -> Tree -> Bool
-- Two trees are equal if and only if:
treeEqual NIL             NIL
    = True
treeEqual (Node x1 l1 r1) (Node x2 l2 r2)
    = x1 == x2 && treeEqual l1 l2 && treeEqual r1 r2
-- and unequal in any other case:
treeEqual _               _
    = False

-- Advanced: We don't about type classes (yet), but this assignment is a very
-- good opportunity to introduce them:
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#t:Eq
-- A type class basically defines a set of functions which must be available if
-- a type is an instance of that type class.
-- The Eq type class has two functions == and /= that test equality and
-- inequality respectively. There can be default implementations of functions.
-- So for example == can defined in terms of /= and vice versa, so that only
-- one of the two has to defined.
instance Eq Tree where
  (==) = treeEqual

