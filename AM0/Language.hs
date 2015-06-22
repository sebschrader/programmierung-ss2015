{-# LANGUAGE MagicHash #-}
module AM0.Language where
import Prelude hiding (EQ,LT,GT)
import Data.Array.IArray(Array)

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

