{
{-# LANGUAGE MagicHash #-}
module AM0.Parser(runParser) where
import Prelude hiding (LT,GT)
import Control.Monad.Trans.Error(runErrorT)
import AM0.Language
import AM0.Lexer (Token(..), runLexer)
import AM0.ParserMonad(ParserError(..), ParserMonad, getNextToken, throwParserError)
}
%name parse
%tokentype { Token }
%error { parseError }
%monad { ParserMonad }
%lexer { getNextToken } { TokenEOF }
%token
  ':'   { TokenColon }
  ';'   { TokenSemicolon }
  int   { TokenInt $$ }
  name  { TokenName $$ }
%%
Statements  : {- empty -}              { [] }
            | Statements Statement     { $2 : $1 }
Statement   : int ':' Instruction ';'  { $3 }
            | Instruction ';'          { $1 }
Instruction : name                     {% zeroOperandInstruction $1 }
            | name int                 {% singleOperandInstruction $1 $2 }
{
parseError tokens = throwParserError $ OtherError ("Parsing failed: " ++ show tokens)

runParser :: String -> Either ParserError [Instruction]
runParser s = case runLexer s (runErrorT parse) of
    Left  msg -> Left $ LexerError msg
    Right a   -> a

zeroOperandInstruction :: String -> ParserMonad Instruction
zeroOperandInstruction "ADD" = return ADD
zeroOperandInstruction "SUB" = return SUB
zeroOperandInstruction "MUL" = return MUL
zeroOperandInstruction "DIV" = return DIV
zeroOperandInstruction "MOD" = return MOD
zeroOperandInstruction "LE"  = return LE
zeroOperandInstruction "LT"  = return LT
zeroOperandInstruction "GE"  = return GE
zeroOperandInstruction "GT"  = return GT
zeroOperandInstruction name  = throwParserError $ UnknownInstruction name

singleOperandInstruction :: String -> Int -> ParserMonad Instruction
singleOperandInstruction "READ"  n = return $ READ (n-1)
singleOperandInstruction "WRITE" n = return $ WRITE (n-1)
singleOperandInstruction "LIT"   n = return $ LIT n
singleOperandInstruction "LOAD"  n = return $ LOAD (n-1)
singleOperandInstruction "STORE" n = return $ STORE (n-1)
singleOperandInstruction "JMP"   n = return $ JMP (n-1)
singleOperandInstruction "JMC"   n = return $ JMC (n-1)
singleOperandInstruction name    _ = throwParserError $ UnknownInstruction name
}

