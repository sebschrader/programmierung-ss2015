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

-- Type classes are defined with the class keyword:
-- A good introduction can be found here:
-- http://en.wikibooks.org/wiki/Haskell/Classes_and_types
class Zippable t where
    genericZipWith :: (a -> b -> c) -> t a -> t b -> t c
    genericZip :: t a -> t b -> t (a,b)
    -- We can give default implementations for the functions of a type class.
    -- For Zippable we can define genericZip in terms of genericZipWith and the
    -- comma constructor of the pair data type:
    -- http://hackage.haskell.org/package/ghc-prim-0.4.0.0/docs/GHC-Tuple.html#t:-40--44--41-
    genericZip = genericZipWith (,)

-- Defining an instance of Zippable for the list type is very simple:
instance Zippable [] where
    -- As stated above, giving a definition for genericZip is no necessary, but
    -- hence we already have one, why not using it?
    genericZip     = zip
    genericZipWith = zipWith


data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

-- Instead of distinguishing all four different combinations of Leaf and Node,
-- we use a little trick:
instance Zippable Tree where
    -- If we have two Nodes we have do have to recurse:
    genericZipWith f (Node xa la ra) (Node xb lb rb)
        = Node (f xa xb) (genericZipWith f la lb) (genericZipWith f ra rb)
    -- In any other case the result will be a single Leaf constructed from the
    -- keys of the two trees:
    genericZipWith f ta              tb
        = Leaf (f (getKey ta) (getKey tb))
        where
            getKey:: Tree c -> c
            getKey (Leaf c    ) = c
            getKey (Node c _ _) = c


-- Advanced: Rose trees
data RoseTree a = RNode a [RoseTree a] deriving Show

-- The implementation for Rose trees is actually much smaller than for binary
-- trees:
instance Zippable RoseTree where
    genericZipWith f (RNode a rtas) (RNode b rtbs)
        = RNode (f a b) (zipWith (genericZipWith f) rtas rtbs)
    -- Using genericZipWith twice would have been possible, but I wanted to
    -- make clear that the first one zips the lists of children and the seconds
    -- one zips two trees.
