module ExprDeepNum (Expr(..),
                    eval) where

-- constructors

data Expr =
    Val Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  deriving (Show)

instance Num Expr where
  fromInteger n = Val n
  e1 + e2       = Add e1 e2
  e1 * e2       = Mul e1 e2
  e1 - e2       = Sub e1 e2

  abs    = undefined
  signum = undefined



-- observer

eval :: Expr -> Integer
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sub e1 e2) = eval e1 - eval e2
