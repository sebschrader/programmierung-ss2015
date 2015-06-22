module AM0.ParserMonad where
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except(ExceptT)
import AM0.Lexer(Lexer, Token(..), lexer)

data ParserError = UnknownInstruction String  -- Name of the instruction
                 | WrongOperandCount String Int Int  -- expected count, got count
                 | OtherError String               -- Other error

instance Show ParserError where
    show (UnknownInstruction name)
        = "Unknown instruction: " ++ name
    show (WrongOperandCount name expected got)
        = "Wrong number of operands for instruction " ++ name ++ ", expected " ++ show expected ++ ", got " ++ show got
    show (OtherError msg) = msg

type ParserMonad = ExceptT ParserError Lexer

getNextToken :: (Token -> ParserMonad a) -> ParserMonad a
getNextToken cont = do
    t <- lift lexer
    cont t

