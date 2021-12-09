{-# LANGUAGE GADTs #-}

module ExprDeepGADTTyped (Expr(..),
                          eval) where

-- Expr a: an expression that, if evaluated, will yield a value of type a

data Expr a where
  ValI   :: Integer                       -> Expr Integer   -- integer literals
  ValB   :: Bool                          -> Expr Bool      -- Boolean literals
  Add    :: Expr Integer -> Expr Integer  -> Expr Integer   -- e1 + e2
  And    :: Expr Bool    -> Expr Bool     -> Expr Bool      -- e1 ∧ e2
  EqZero :: Expr Integer                  -> Expr Bool      -- e ≟ 0
  If     :: Expr Bool -> Expr a -> Expr a -> Expr a         -- if e1 then e2 else e3

instance Show (Expr a) where
  show (ValI n)     = show n
  show (ValB b)     = show b
  show (Add e1 e2)  = show e1 ++ " + " ++ show e2
  show (And e1 e2)  = show e1 ++ " ∧ " ++ show e2
  show (EqZero e)   = show e  ++ " == 0"
  show (If p e1 e2) = "if " ++ show p ++ " then " ++ show e1 ++ " else " ++ show e2


-- NB: this is *typed* evaluation of expressions:
eval :: Expr a -> a
eval (ValI n)     = n
eval (ValB b)     = b
eval (Add e1 e2)  = (eval e1) + (eval e2)
eval (And e1 e2)  = (eval e1) && (eval e2)
eval (EqZero e)   = eval e == 0
eval (If p e1 e2) = if eval p then eval e1 else eval e2
