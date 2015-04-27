module A3
( Tree
, tree1
, countLeaf
, leafsToList
) where

data Tree = Leaf Int | Branch Tree Tree deriving Show

-- a
tree1 = Branch ( Branch (Leaf 1) (Leaf 2)) (Branch  (Branch (Leaf 3) (Leaf 4)) (Leaf 5))

--b
-- the patterns are now the different type constructors.
countLeaf :: Tree -> Int
countLeaf (Leaf a)            = 1
countLeaf (Branch left right) = countLeaf left + countLeaf right

leafsToList :: Tree -> [Int]
leafsToList (Leaf a)            = [a]
leafsToList (Branch left right) = leafsToList left ++ (leafsToList right

