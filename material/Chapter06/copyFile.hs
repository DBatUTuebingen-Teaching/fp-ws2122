import System.IO (readFile, writeFile, getLine, putStrLn)

copyFile :: FilePath -> FilePath -> IO Int
copyFile from to = do
  content <- readFile from
  writeFile to content
  return (length content)

main :: IO ()
main = do
  putStrLn "Which file do you want to copy?"
  from <- getLine
  putStrLn "Where do you want to copy it to?"
  to <- getLine
  n <- copyFile from to
  putStrLn ("Copied " ++ show n ++ " characters.")
