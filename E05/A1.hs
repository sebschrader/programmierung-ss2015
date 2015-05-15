module E05.A1 where

-- We have already seen zipWith in the last exercise, were we used it to
-- implement pleat'. See the Haskell docs for more info:
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:zipWith
-- Here's an implementation of zipWith:
{-
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []
-}

class Zippable t where
    genericZipWith :: (a -> b -> c) -> t a -> t b -> t c
    genericZip :: t a -> t b -> t (a,b)
    genericZip = genericZipWith (,)

-- Defining an instance of Zippable for the list type is very simple:
instance Zippable [] where
    genericZip     = zip
    genericZipWith = zipWith


data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

treeZipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
treeZipWith f (Leaf xa)       (Leaf xb)       = 
    Leaf (f xa xb)
treeZipWith f (Node xa la ra) (Node xb lb rb) = 
    Node (f xa xb) (treeZipWith f la lb) (treeZipWith f la lb) 
treeZipWith f (Leaf xa)       (Node xb la ra) =
    Leaf (f xa xb)
treeZipWith f (Node xa la ra) (Leaf xb)       =
    Leaf (f xa xb)

instance Zippable Tree where
    genericZipWith = treeZipWith
