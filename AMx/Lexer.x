{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module AMx.Lexer (Lexer, Token(..), lexer, getPosition, runLexer) where
}

%wrapper "monad"

$digit = [0-9]          -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$white = [\ \t\f\v]     -- whitespace
$sep   = [\;\r\n]       -- instruction separators

tokens :-
  $sep+     { mkToken TokenSeparator }
  :         { mkToken TokenColon }
  $white+   ;
  $digit+   { getInteger }
  $alpha+   { getName }
{

mkToken :: Token -> AlexInput -> Int -> Alex Token
mkToken t = \_ _ -> return t

getInteger :: AlexInput -> Int -> Alex Token
getInteger (_, _, _, s) n = return $ TokenInt $ read $ take n s

getName :: AlexInput -> Int -> Alex Token
getName (_, _, _, s) n = return $ TokenName $ take n s

data Token = TokenName String
           | TokenColon
           | TokenComma
           | TokenOpenParenthesis
           | TokenCloseParenthesis
           | TokenSeparator
           | TokenInt Int
           | TokenEOF
           deriving (Eq,Show)

alexEOF :: Alex Token
alexEOF = return TokenEOF

type Lexer = Alex

lexer :: Lexer Token
lexer = alexMonadScan

runLexer :: String -> Lexer a -> Either String a
runLexer = runAlex

getPosition :: Lexer (Int, Int)
getPosition = do
    ((AlexPn _ line col), _, _, _) <- alexGetInput
    return (line, col)

}

