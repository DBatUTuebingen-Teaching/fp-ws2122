f :: IO Integer
f = do
  putStrLn "Running f ..."
  return 1893
  return 1904

main :: IO ()
main = do
  x <- f
  print x
