import System.IO (readFile, writeFile, getLine, putStrLn)

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = do
  content <- readFile from
  putStrLn ("Read " ++ from ++ ", now writing " ++ to)
  writeFile to content

ignore :: a -> IO ()
ignore unused = putStrLn "Ignoring my argument!"

don'tignore :: IO () -> IO ()
don'tignore action = do
  putStrLn "About to execute the action..."
  action
  putStrLn "Executed the action."

main :: IO ()
main = do
  putStrLn "Which file do you want to copy?"
  from <- getLine
  putStrLn "Where do you want to copy it to?"
  to <- getLine
  don'tignore (copyFile from to)
  --      └──────┬───────┘
  --    constructs the action to copy the file, but does
  --    NOT execute it (just passes the action around)
  putStrLn "Copying done?!"
