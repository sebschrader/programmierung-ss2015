module A4
( Expr
, eval
, display
) where

data Expr
    = Lit Int
    | Var Char
    | Add Expr Expr
    | Mul Expr Expr
    | Exp Expr Int

-- type only says that the name Assignment
-- stands for function with type (Char -> Int)
type Assignment = Char -> Int

-- You see Expr is the type of Expr
-- but to create a Element of type Expr you call:
-- Lit, Var, Add, Mul or Exp
example1 :: Expr
example1 = (Exp (Add (Lit 4) (Var 'a')) 2)

-- a
-- the type of the eval function could also be written
-- Expr -> (Char -> Int) -> Int
eval :: Expr -> Assignment -> Int
eval (Lit a)   _ = a
eval (Var a)   f = f a
eval (Add a b) f = (eval a f) + (eval b f)
eval (Mul a b) f = (eval a f) * (eval b f)
eval (Exp a 0) _ = 1
eval (Exp a b) f = (eval a f) * (eval (Exp a (b-1)) f)

-- b
-- additional built-in functions: show
-- show makes a something marked with "derving show" to a String
display :: Expr -> String
display (Lit a)   = show a
display (Var a)   =  [a]
display (Add a b) =  "(" ++ (display a) ++ " + " ++ (display b) ++ ")"
display (Mul a b) = (display a) ++ " * " ++ (display b)
display (Exp a b) =  "(" ++ (display a) ++ ")^" ++ (show b)

