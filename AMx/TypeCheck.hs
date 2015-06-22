{-# LANGUAGE GADTs #-}
module AMx.TypeCheck where
import Prelude hiding (LT,GT)
import Data.Char(toUpper)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import AMx.Language(Argument(..), InstructionSpecification(..), Type(..))
import AMx.ParserMonad(ParserError(..), ParserMonad, Reason(..), getSpecifications, throwParserError)

getSpecification :: String -> ParserMonad i (InstructionSpecification i)
getSpecification name = do
        specs <- getSpecifications
        case Map.lookup upper_name specs of
            Nothing   -> throwParserError $ UnknownInstruction upper_name
            Just spec -> return spec
    where upper_name = map toUpper name

checkNullaryInstruction :: (InstructionSpecification a) -> ParserMonad i a
checkNullaryInstruction (Nullary _name f    ) = return f
checkNullaryInstruction (Unary    name _ _  ) = throwParserError $ WrongOperandCount name 1 0
checkNullaryInstruction (Binary   name _ _ _) = throwParserError $ WrongOperandCount name 2 0

checkUnaryInstruction :: Argument -> (InstructionSpecification a) -> ParserMonad i a
checkUnaryInstruction _   (Nullary  name _    ) = throwParserError $ WrongOperandCount name 0 1
checkUnaryInstruction arg (Unary   _name f t  ) = checkType arg t >>= \v -> return $ f v
checkUnaryInstruction _   (Binary   name _ _ _) = throwParserError $ WrongOperandCount name 2 1

checkBinaryInstruction :: Argument -> Argument -> (InstructionSpecification a) -> ParserMonad i a
checkBinaryInstruction _    _    (Nullary  name _            ) = throwParserError $ WrongOperandCount name 0 2
checkBinaryInstruction _    _    (Unary    name _ _          ) = throwParserError $ WrongOperandCount name 1 2
checkBinaryInstruction arg1 arg2 (Binary  _name f type1 type2) = do
    value1 <- checkType arg1 type1
    value2 <- checkType arg2 type2
    return $ f value1 value2

checkType :: Argument -> Type a -> ParserMonad i a
checkType (IntArgument v)    IntType    = return v
checkType (StringArgument v) StringType = return v
checkType _                  _          = throwParserError $ OtherError "Wrong type."
