import ExprDeep

-- e1 ≡ if 0 + 0 == 0 then 42 else 43
e1 :: Expr
e1 = If (EqZero (Add (ValI 0) (ValI 0)))
        (ValI 42)
        (ValI 43)

-- e2 ≡ True == 0
e2 :: Expr
e2 = EqZero (ValB True)

main :: IO ()
main = do
  print e1
  print e2
  print $ eval e1
  -- print $ eval e2    -- ⚠️ leads to a runtime pattern match error

