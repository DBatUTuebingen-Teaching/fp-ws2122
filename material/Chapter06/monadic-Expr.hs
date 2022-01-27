import Data.Maybe (maybe)

-- Exc a is either an exception (Exc err) or a value (Val x)
data Exc a =
    Exc { exc :: Error }
  | Val { val :: a }
  deriving (Show)

type Error = String

instance Monad Exc where
  return x = Val x

  Exc err >>= g = Exc err
  Val x   >>= g = g x

-----------------------------------------------------------------------

data Expr =
    Lit Integer
  | App Op Expr Expr
  deriving (Show)

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

eval :: Expr -> Exc Integer
eval (Lit n)        = Val n
eval (App op e1 e2) = do f  <- operator op
                         x1 <- eval e1
                         x2 <- eval e2
                         return $ f x1 x2

operator :: Op -> Exc (Integer -> Integer -> Integer)
operator op = maybe (Exc ("unknown operator " ++ show op)) Val $ lookup op ops
  where
    ops = [(Add, (+)),
           (Sub, (-)),
           (Div, div)
          ]

-----------------------------------------------------------------------

main :: IO ()
main = print $ eval (App Div (Lit 41) (Lit 0))
