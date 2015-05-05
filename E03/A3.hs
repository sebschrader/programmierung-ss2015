module E03.A3 where

data Tree = Branch Int Tree Tree | NIL deriving Show

good_example = (Branch 2 (Branch 1 NIL NIL) NIL) :: Tree
bad_example  = (Branch 1 (Branch 2 NIL NIL) NIL) :: Tree

isHeap :: Tree -> Bool
isHeap NIL
    = True
isHeap (Branch x NIL               NIL)
    = True
isHeap (Branch x NIL               r@(Branch xr _ _))
    = x >= xr && isHeap r
isHeap (Branch x l@(Branch xl _ _) NIL)
    = x >= xl && isHeap l
isHeap (Branch x l@(Branch xl _ _) r@(Branch xr _ _))
    = x >= xl && x >= xr && isHeap l && isHeap r

isHeap' :: Tree -> Bool
isHeap' (NIL) = True
isHeap' (Branch a l r) = (isBigger a l) && (isBigger a r)
    where isBigger :: Int -> Tree -> Bool
          isBigger x (NIL) = True
          isBigger x (Branch a l r) = a < x
