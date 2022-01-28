
check :: String -> String -> IO Bool
check q a = putStrLn (q ++ " Wait...") >>
            return (a == "42")

main :: IO ()
main =
  putStrLn "Please give a question:" >>
  getLine >>= \q ->
  putStrLn "And an answer:" >>
  getLine >>= \a ->
  if last q == '?'
    then check q a >>= \res ->
         putStrLn $ "Your answer is " ++ show res
    else putStrLn "Your question should end with an ?. Retry!" >>
         main

-- Or using do-notation:

main2 :: IO ()
main2 = do
  putStrLn "Please give a question:"
  q <- getLine
  putStrLn "And an answer:"
  a <- getLine
  if last q == '?'
  then do
       res <- check q a
       putStrLn $ "Your answer is " ++ show res
  else do
       putStrLn "Your question should end with an ?. Retry!"
       main2
