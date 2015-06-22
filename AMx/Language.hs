{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module AMx.Language where
import Prelude hiding (EQ,LT,GT)
import Data.Array.IArray(Array)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

data Instruction = READ Int
                 | WRITE Int
                 | LIT Int
                 | LOAD Int
                 | STORE Int
                 | ADD
                 | SUB
                 | MUL
                 | DIV
                 | MOD
                 | EQ
                 | LE
                 | LT
                 | GE
                 | GT
                 | JMP Int
                 | JMC Int
                 deriving (Show, Eq)

type Program = Array Int Instruction

data InstructionSpecification where
    Nullary :: String -> Instruction -> InstructionSpecification
    Unary :: forall a . String -> (a -> Instruction) -> (Type a) -> InstructionSpecification
    Binary :: forall a b . String -> (a -> b -> Instruction) -> (Type a) -> (Type b) -> InstructionSpecification

data Argument = IntArgument Int | StringArgument String

data Type a where
    IntType :: Type Int
    StringType :: Type String

fromList :: [InstructionSpecification] -> Map String InstructionSpecification
fromList = Map.fromList . map toTuple
    where toTuple spec@(Nullary name _    ) = (name, spec)
          toTuple spec@(Unary   name _ _  ) = (name, spec)
          toTuple spec@(Binary  name _ _ _) = (name, spec)
