import Control.Monad
import Control.Applicative

data IntOp = Mul | Div | Eq
     deriving (Show)

data BoolOp = And | Or
     deriving (Show)

data Val = IntV Int | BoolV Bool
     deriving (Show)

data Expr = Lit Val
          | AppInt IntOp Expr Expr
          | AppBool BoolOp Expr Expr
          | Var String
          | Let (String, Expr) Expr
     deriving (Show)

-- | Environment mapping variable names to values
type Env = [(String, Val)]

-- | Evaluation in an environment.
newtype Eval a = E { runEval :: Env -> Maybe a }
