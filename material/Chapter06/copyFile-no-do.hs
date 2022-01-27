import System.IO (readFile, writeFile, getLine, putStrLn)

copyFile :: FilePath -> FilePath -> IO Int
copyFile from to =
  readFile from         >>= \content ->
  writeFile to content  >>= \_       ->
  return (length content)

main :: IO ()
main =
  putStrLn "Which file do you want to copy?"  >>= \_ ->
  getLine                                     >>= \from ->
  putStrLn "Where do you want to copy it to?" >>= \_ ->
  getLine                                     >>= \to ->
  copyFile from to                            >>= \n ->
  putStrLn ("Copied " ++ show n ++ " characters.")
