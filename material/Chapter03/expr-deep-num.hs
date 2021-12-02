import ExprDeepNum

e1 :: Expr
e1 = 8 * 7 - 14
-- e1 = Sub (Mul (Val 8) (Val 7)) (Val 14)


main :: IO ()
main = do
  print $ e1
  print $ eval e1

