module ExprDeep (Expr(..),
                 eval) where

data Expr =
    ValI Integer           -- integer literals
  | ValB Bool              -- Boolean literals
  | Add Expr Expr          -- e1 + e2
  | And Expr Expr          -- e1 ∧ e2
  | EqZero Expr            -- e ≟ 0
  | If Expr Expr Expr      -- if e1 then e2 else e3


instance Show Expr where
  show (ValI n)     = show n
  show (ValB b)     = show b
  show (Add e1 e2)  = show e1 ++ " + " ++ show e2
  show (And e1 e2)  = show e1 ++ " ∧ " ++ show e2
  show (EqZero e)   = show e  ++ " == 0"
  show (If p e1 e2) = "if " ++ show p ++ " then " ++ show e1 ++ " else " ++ show e2


-- evaluation of an expression either yields
-- an integer (Left n) or a Boolean (Right b)
eval :: Expr -> Either Integer Bool
eval (ValI n)     = Left n
eval (ValB b)     = Right b
eval (Add e1 e2)  = case (eval e1, eval e2) of
                      (Left n1, Left n2) -> Left (n1 + n2)
                      (_      , _      ) -> error "type error in addition"
eval (And e1 e2)  = case (eval e1, eval e2) of
                      (Right b1, Right b2) -> Right (b1 && b2)
eval (EqZero e)   = case eval e of
                      Left n -> Right (n == 0)
eval (If p e1 e2) = case eval p of
                      Right b -> eval (if b then e1 else e2)
