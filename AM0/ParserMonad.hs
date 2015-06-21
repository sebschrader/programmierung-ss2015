module AM0.ParserMonad where
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Error(Error, ErrorT, strMsg, throwError)
import AM0.Lexer(Lexer, Token(..), lexer, runLexer)


data ParserError = UnknownInstruction String        -- Name of the instruction
                 | WrongOperandCount String Int Int -- expected count, got count
                 | LexerError String                -- Error from the Lexer
                 | OtherError String                -- Other error

instance Error ParserError where
    strMsg s = OtherError s

instance Show ParserError where
    show (UnknownInstruction name)
        = "Unknown instruction: " ++ name
    show (WrongOperandCount name expected got)
        = "Wrong number of operands for instruction " ++ name ++ ", expected " ++ show expected ++ ", got " ++ show got
    show (LexerError msg) = msg
    show (OtherError msg) = msg

type ParserMonad = ErrorT ParserError Lexer

throwParserError :: ParserError -> ParserMonad a
throwParserError = throwError

getNextToken :: (Token -> ParserMonad a) -> ParserMonad a
getNextToken cont = do
    t <- lift lexer
    cont t

