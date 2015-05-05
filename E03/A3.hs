module E03.A3 where

data Tree = Branch Int Tree Tree | NIL deriving Show

good_example = (Branch 2 (Branch 1 NIL NIL) NIL) :: Tree
bad_example = (Branch 1 (Branch 2 NIL NIL) NIL) :: Tree

isHeap :: Tree -> Bool
isHeap NIL 
        = True
isHeap (Branch x NIL                  NIL)
        = True
isHeap (Branch x NIL                  tr@(Branch xr rr lr)) 
	= x > xr && isHeap tr
isHeap (Branch x tl@(Branch xl rl ll) NIL) 
	= x > xl && isHeap tl
isHeap (Branch x tl@(Branch xl rl ll) tr@(Branch xr rr lr))
        = x > xl && x > xr && isHeap tl && isHeap tr

isHeap' :: Tree -> Bool
isHeap' (NIL) = True
isHeap' (Branch a l r) = (isBigger a l) && (isBigger a r)
    where isBigger :: Int -> Tree -> Bool
          isBigger x (NIL) = True
          isBigger x (Branch a l r) = a < x
