module E05.A1 where


class Zippable t where
    genericZipWith :: (a -> b -> c) -> t a -> t b -> t c
    genericZip :: t a -> t b -> t (a,b)
    genericZip = genericZipWith (,)

listZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
listZipWith f (xa : xas) (xb : xbs) = (f xa xb) : listZipWith f xas xbs
listZipWith _ _          _          = []

instance Zippable [] where
    genericZipWith = listZipWith


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
