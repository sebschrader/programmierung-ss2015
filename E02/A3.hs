module A3
( Tree
, tree1
, countLeaf
, leafsToList
) where

-- We define a new data type with the data keyword which has two type
-- constructors Leaf and Branch
-- deriving Show instructs the compiler to automatically generate an instance
-- for the Show type class (more on type classes and instances in later
-- exercises). With show we can easily print values.
data Tree = Leaf Int | Branch Tree Tree deriving Show

-- (a)
tree1 :: Tree
tree1 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch  (Branch (Leaf 3) (Leaf 4)) (Leaf 5))

-- (b)
-- Using pattern matching we can distinguish the different type constructors.
countLeaf :: Tree -> Int
countLeaf (Leaf a)            = 1
countLeaf (Branch left right) = countLeaf left + countLeaf right

leafsToList :: Tree -> [Int]
leafsToList (Leaf a)            = [a]
leafsToList (Branch left right) = leafsToList left ++ (leafsToList right)

