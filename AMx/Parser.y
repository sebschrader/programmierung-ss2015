{
{-# LANGUAGE MagicHash #-}
module AMx.Parser(runParser) where
import Control.Monad.Trans.Error(runErrorT)
import Control.Monad.Trans.Reader(runReaderT)
import Data.Map.Strict(Map)
import AMx.Language(Argument(..), InstructionSpecification, Program, programFromList)
import AMx.Lexer (Token(..), runLexer)
import AMx.ParserMonad(ParserError(..), ParserMonad, Reason(..), getNextToken, throwParserError)
import AMx.TypeCheck
}
%name parse0
%tokentype { Token }
%error { parseError }
%monad { ParserMonad i }
%lexer { getNextToken } { TokenEOF }
%token
  '('   { TokenOpenParenthesis }
  ')'   { TokenCloseParenthesis }
  ':'   { TokenColon }
  ';'   { TokenSeparator }
  ','   { TokenComma }
  int   { TokenInt $$ }
  name  { TokenName $$ }
%%
Statements  : {- empty -}                     { [] }
            | Statements Statement            { $2 : $1 }
Statement   : int ':' Instruction Separators  { $3 }
            | Instruction Separators          { $1 }
Instruction : name Arguments                  {% getSpecification $1 >>= $2 }
Arguments   : '(' Arguments0 ')'              { $2 }
            | Arguments0                      { $1 }
Arguments0  : {- empty -}                     { checkNullaryInstruction }
            | Argument                        { checkUnaryInstruction $1 }
            | Argument ',' Argument           { checkBinaryInstruction $1 $3 }
Argument    : int                             { IntArgument $1 }
            | name                            { StringArgument $1 }
Separators  : ';'                             { () }
            | Separators ';'                  { () }
{
parseError tokens = throwParserError $ OtherError ("Parsing failed: " ++ show tokens)

parse :: ParserMonad i (Program i)
parse = parse0 >>= return . programFromList . reverse

runParser :: String -> Map String (InstructionSpecification i) -> Either ParserError (Program i)
runParser s is = case runLexer s $ runReaderT (runErrorT parse) is of
    Left  msg -> Left $ ParserError Nothing (LexerError msg)
    Right a   -> a
}
