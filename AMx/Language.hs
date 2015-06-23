{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module AMx.Language where
import Data.Array.IArray(Array)
import qualified Data.Array.IArray as Array
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

type Program i = Array Int i

data InstructionSpecification i where
    Nullary :: String -> i -> InstructionSpecification i
    Unary :: String -> (a -> i) -> (Type a) -> InstructionSpecification i
    Binary :: String -> (a -> b -> i) -> (Type a) -> (Type b) -> InstructionSpecification i

data Argument = IntArgument Int | StringArgument String

data Type a where
    IntType :: Type Int
    StringType :: Type String

programFromList :: [i] -> Program i
programFromList l = Array.listArray (1, length l) l

getInstruction :: Program i -> Int -> i
getInstruction = (Array.!)

programBounds :: Program i -> (Int, Int)
programBounds = Array.bounds

fromList :: [InstructionSpecification i] -> Map String (InstructionSpecification i)
fromList = Map.fromList . map toTuple
    where toTuple spec@(Nullary name _    ) = (name, spec)
          toTuple spec@(Unary   name _ _  ) = (name, spec)
          toTuple spec@(Binary  name _ _ _) = (name, spec)
