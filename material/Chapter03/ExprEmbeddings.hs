-- The following language extension is needed to allow instance declarations
-- that deviate from the usual forms (with T denoting a type [constructor]
-- and a, b, c, ... denoting type *variables*):
--
--    instance C T where ...
--    instance C (T a b c ...) where ...
--
-- With FlexibleInstances, we can define instances of class C for *concrete*
-- types.

{-# LANGUAGE FlexibleInstances #-}


module ExprEmbeddings (Expr,
                       Env,
                       AST(..),
                       val,
                       add,
                       var,
                       bnd) where


-- A class for a simple expression language that features ...
class Expr a where
  val :: Integer -> a            -- literal integers
  add :: a -> a -> a             -- addition
  var :: String -> a             -- variable references
  bnd :: (String, a) -> a -> a   -- let bindings


-- Three instances that define two shallow and one deep
-- embedding of the expression language into Haskell

-----------------------------------------------------------------------
-- Shallow embedding #1
-- Expression evaluation based on environments that binds names to values

type Env = [(String, Integer)]  -- variable environment: {v₁↦x₁, v₂↦x₂, ...}

instance Expr (Env -> Integer) where
  val n         = \_ -> n
  add e1 e2     = \e -> e1 e + e2 e
  var v         = \e -> case lookup v e of
                          Just n  -> n
                          Nothing -> error (v ++ " is unknown")
  bnd (v,e1) e2 = \e -> e2 ((v,e1 e):e)
  --                        ───────
  --                        e' = e + {v↦e1}

-----------------------------------------------------------------------
-- Shallow embedding #2
-- Expression printing

instance Expr String where
  val n         = show n
  add e1 e2     = e1 ++ " + " ++ e2
  var v         = v
  bnd (v,e1) e2 = "let " ++ v ++ " = " ++ e1 ++ " in (" ++ e2 ++ ")"

-----------------------------------------------------------------------
-- Deep embedding
-- Building abstract syntax trees for expressions

data AST a = Val a
           | Add (AST a) (AST a)
           | Var String
           | Let String (AST a) (AST a)
  deriving (Eq, Show)

instance Expr (AST Integer) where
  val n         = Val n
  add e1 e2     = Add e1 e2
  var v         = Var v
  bnd (v,e1) e2 = Let v e1 e2
