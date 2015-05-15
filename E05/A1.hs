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

instance Zippable Tree where
    genericZipWith f (Leaf xa      ) (Leaf xb      )
        = Leaf (f xa xb)
    genericZipWith f (Node xa la ra) (Node xb lb rb)
        = Node (f xa xb) (genericZipWith f la lb) (genericZipWith f ra rb)
    genericZipWith f (Leaf xa      ) (Node xb la ra)
        = Leaf (f xa xb)
    genericZipWith f (Node xa la ra) (Leaf xb      )
        = Leaf (f xa xb)
