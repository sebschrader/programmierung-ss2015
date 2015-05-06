module E04.A1 where

data Tree = Leaf | Branch Tree Tree deriving (Show, Eq)

mkBalanced :: Int -> [Tree]
mkBalanced 0 = []
mkBalanced 1 = [Leaf]
mkBalanced n = [Branch l r | (hl, hr) <- [(n-2,n-1), (n-1,n-2), (n-1,n-1)]
                            , l <- mkBalanced hl
                            , r <- mkBalanced hr]
