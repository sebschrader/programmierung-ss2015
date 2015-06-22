module AMx.ParserMonad where
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Error(Error, ErrorT, runErrorT, strMsg, throwError)
import Control.Monad.Trans.Reader(ReaderT)
import Data.List(intercalate)
import AMx.Language
import AMx.Lexer(Lexer, Token(..), getPosition, lexer, runLexer)

data Position = Position { getName :: String, getLine :: Int, getColumn :: Int }

instance Show Position where
    show (Position name line column) = intercalate ":" [name, show line, show column]

data Reason = UnknownInstruction String        -- Name of the instruction
            | WrongOperandCount String Int Int -- expected count, got count
            | LexerError String                -- Error from the Lexer
            | OtherError String                -- Other error

data ParserError = ParserError (Maybe Position) Reason

instance Error ParserError where
    strMsg s = ParserError Nothing (OtherError s)

instance Show Reason where
    show (UnknownInstruction name)
        = "Unknown instruction: " ++ name
    show (WrongOperandCount name expected got)
        = "Wrong number of operands for instruction " ++ name ++ ", expected " ++ show expected ++ ", got " ++ show got
    show (LexerError msg) = msg
    show (OtherError msg) = msg

instance Show ParserError where
    show (ParserError pos err) = case pos of
        Nothing -> show err
        Just p  -> show p ++ ": " ++ show err

type ParserMonad = ErrorT ParserError (ReaderT [InstructionSpecification] Lexer)

throwParserError :: Reason -> ParserMonad a
throwParserError e = do
    (line, column) <- lift $ lift getPosition
    throwError $ ParserError (Just $ Position "test" line column) e

getNextToken :: (Token -> ParserMonad a) -> ParserMonad a
getNextToken cont = do
    t <- lift $ lift lexer
    cont t
