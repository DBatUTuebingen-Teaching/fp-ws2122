-- Interactively copy a file (uses conditional in do to handle error)

import System.IO (readFile, writeFile, getLine, putStrLn)
import System.Directory (doesFileExist)   -- doesFileExist :: IO Bool

copyFile :: FilePath -> FilePath -> IO Int
copyFile from to = do
  ex <- doesFileExist from
  if ex then do
    content <- readFile from
    writeFile to content
    return (length content)
  else do
    putStrLn ("File " ++ from ++ " does not exist.")
    return 0

main :: IO ()
main = do
  putStrLn "Which file do you want to copy?"
  from <- getLine
  putStrLn "Where do you want to copy it to?"
  to <- getLine
  n <- copyFile from to
  putStrLn ("Copied " ++ show n ++ " characters.")
