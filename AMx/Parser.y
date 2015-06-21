{
{-# LANGUAGE MagicHash #-}
module AMx.Parser(runParser) where
import Control.Monad.Trans.Error(runErrorT)
import AMx.Lexer (Token(..), runLexer)
import AMx.TypeCheck
import AMx.ParserMonad(ParserError(..), ParserMonad, Reason(..), getNextToken, throwParserError)
import AMx.Language(Instruction)
}
%name parse
%tokentype { Token }
%error { parseError }
%monad { ParserMonad }
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

runParser :: String -> Either ParserError [Instruction]
runParser s = case runLexer s (runErrorT parse) of
    Left  msg -> Left $ ParserError Nothing (LexerError msg)
    Right a   -> a
}
