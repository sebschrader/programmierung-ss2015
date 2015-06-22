{-# LANGUAGE GADTs #-}
module AMx.TypeCheck where
import Prelude hiding (LT,GT)
import Data.Char(toUpper)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import AMx.Language
import AMx.ParserMonad(ParserError(..), ParserMonad, Reason(..), throwParserError)

instructions :: Map String InstructionSpecification
instructions = Map.fromList [ ("ADD", Nullary "ADD" ADD)
                            , ("SUB", Nullary "SUB" SUB)
                            , ("MUL", Nullary "MUL" MUL)
                            , ("DIV", Nullary "DIV" DIV)
                            , ("MOD", Nullary "MOD" MOD)
                            , ("GE", Nullary "GE" GE)
                            , ("GT", Nullary "GT" GT)
                            , ("LE", Nullary "LE" LE)
                            , ("LT", Nullary "LT" LT)
                            , ("READ", Unary "READ" READ IntType)
                            , ("WRITE", Unary "WRITE" WRITE IntType)
                            , ("LOAD", Unary "LOAD" LOAD IntType)
                            , ("STORE", Unary "STORE" STORE IntType)
                            , ("LIT", Unary "LIT" LIT IntType)
                            , ("JMP", Unary "JMP" JMP IntType)
                            , ("JMC", Unary "JMC" JMC IntType)
                            ]

getSpecification :: String -> ParserMonad InstructionSpecification
getSpecification name = case Map.lookup upper_name instructions of
        Nothing   -> throwParserError $ UnknownInstruction upper_name
        Just spec -> return spec
    where upper_name = map toUpper name

checkNullaryInstruction :: InstructionSpecification -> ParserMonad Instruction
checkNullaryInstruction (Nullary _name f    ) = return f
checkNullaryInstruction (Unary    name _ _  ) = throwParserError $ WrongOperandCount name 1 0
checkNullaryInstruction (Binary   name _ _ _) = throwParserError $ WrongOperandCount name 2 0

checkUnaryInstruction :: Argument -> InstructionSpecification -> ParserMonad Instruction
checkUnaryInstruction _   (Nullary  name _    ) = throwParserError $ WrongOperandCount name 0 1
checkUnaryInstruction arg (Unary   _name f t  ) = checkType arg t >>= \v -> return $ f v
checkUnaryInstruction _   (Binary   name _ _ _) = throwParserError $ WrongOperandCount name 2 1

checkBinaryInstruction :: Argument -> Argument -> InstructionSpecification -> ParserMonad Instruction
checkBinaryInstruction _    _    (Nullary  name _            ) = throwParserError $ WrongOperandCount name 0 2
checkBinaryInstruction _    _    (Unary    name _ _          ) = throwParserError $ WrongOperandCount name 1 2
checkBinaryInstruction arg1 arg2 (Binary  _name f type1 type2) = do
    value1 <- checkType arg1 type1
    value2 <- checkType arg2 type2
    return $ f value1 value2

checkType :: Argument -> Type a -> ParserMonad a
checkType (IntArgument i)    IntType    = return i
checkType (StringArgument s) StringType = return s
checkType _                  _          = throwParserError $ OtherError "Wrong type."
