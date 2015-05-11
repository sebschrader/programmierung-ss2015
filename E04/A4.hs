module E04.A4 where

data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving Show

-- (a)
inorder :: Tree a -> [a]
inorder (Leaf   a    ) = [a]
inorder (Branch a l r) = inorder l ++ [a] ++ inorder r

-- (b)
-- Like map is transforming a list by applying a given function to all
-- elements, we define a similar function mapTree for our Tree type.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf   a    ) = Leaf   (f a)
mapTree f (Branch a l r) = Branch (f a) (mapTree f l) (mapTree f r)

-- Advanced:
-- The concept of mapping is not only applicable to list or trees. Any data
-- container can implement this functionality. In Haskell there is typeclass
-- Functor for this specific purpose:
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#t:Functor
-- We can therefore make Tree an instance of Functor.
instance Functor Tree where
    fmap = mapTree

