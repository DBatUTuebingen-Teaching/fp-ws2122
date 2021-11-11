-- Abstract syntax tree for arithmetic expressions of literals
data Exp a = Lit a                 -- constant
           | Add (Exp a) (Exp a)   -- e1 + e2
           | Sub (Exp a) (Exp a)   -- e1 - e2
           | Mul (Exp a) (Exp a)   -- e1 * e2
  deriving (Show)

-- Evaluate (interpret) an arithmetic expression
eval :: Num a => Exp a -> a
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Sample expressions
ex1 :: Exp Integer
ex1 = Add (Mul (Lit 5) (Lit 8)) (Lit 2)  --  (5 * 8) + 2

ex2 :: Exp Float
ex2 = Sub (Lit 42.1) (Lit 0.1)           -- 42.1 - 0.1

main :: IO ()
main = do
  print $ ex1
  print $ ex2
  print $ eval ex1
  print $ eval ex2

