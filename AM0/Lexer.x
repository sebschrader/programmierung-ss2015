{
module AM0.Lexer (Lexer, Token(..), lexer, runLexer) where
}

%wrapper "monad"

$digit = [0-9]          -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  \;        { mkToken TokenSemicolon }
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
           | TokenSemicolon
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
}

