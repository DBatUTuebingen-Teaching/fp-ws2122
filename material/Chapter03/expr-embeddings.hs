import ExprEmbeddings

-- Sample expression:
--
-- let x = 3 in (let x = 0 in x) + x

-- This expression has any type a as long as type a is an instance of
-- (i.e. an embedding of) Expr.  Choosing type a defines what we would
-- like to do with the expression:
-- • a ≡ Env -> Integer: evaluate it
-- • a ≡ String:         display it
-- • a ≡ AST Integer:    simplify it
-- • a ≡ ...
prog :: Expr a => a
prog = bnd ("x", val 3) (add (bnd ("x", val 0) (var "x")) (var "x"))


-- Since the deep embedding allows us to inspect the structure
-- of the expression (i.e. its AST) we can manipulate and simplify
-- it.
--
-- NB: omitting `repeat' will miss simplifcation opportunities,
--     consider: add (bnd ("x", val 0) (var "x")) (val 42) ≡ (let x = 0 in x) + 42

simplify :: AST Integer -> AST Integer
simplify e = repeat rewrite e
  where
    repeat :: Eq a => (a -> a) -> a -> a
    repeat f = until (\x -> f x == x) f

    rewrite :: AST Integer -> AST Integer
    rewrite (Add (Val 0) e2)              = rewrite e2
    rewrite (Add e1 (Val 0))              = rewrite e1
    rewrite (Add e1 e2)                   = Add (rewrite e1) (rewrite e2)
    rewrite (Let _ e1 e2@(Val _))         = rewrite e2
    rewrite (Let v e1 (Var v')) | v == v' = rewrite e1
    rewrite (Let v e1 e2)                 = Let v (rewrite e1) (rewrite e2)
    rewrite e                             = e


-- Unless the context prescribes it, we need to be explicit about
-- which instance (embedding) we're after:
main :: IO ()
main = do
  print $ (prog :: Env -> Integer) []  --  [] is the empty environment ∅

  print   (prog :: String)

  print   (prog :: AST Integer)
  print $ simplify prog
