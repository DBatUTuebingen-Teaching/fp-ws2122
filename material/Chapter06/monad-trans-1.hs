import Data.List.Split (splitOn)
import Data.Either (either)

-- Execption that can occur during login
--
data LoginError = InvalidEmail
  deriving Show

-- Verify that e-mail address contains exactly one '@'
--
checkEmail :: String -> Either LoginError String
checkEmail email = case splitOn "@" email of
                     [_name, _domain] -> Right email
                     _                -> Left InvalidEmail


-- Now process the result returned by checkEmail, we need to
-- handle the success and exception cases:

-- Variant (1): pattern matching
--
printResult :: Either LoginError String -> IO ()
printResult email =
  putStrLn $ case email of
               Right addr        -> "Address: " ++ addr
               Left InvalidEmail -> "Invalid e-mail address entered."

-- Variant (2): rely on combinator 'either' which encapsulates the
--              pattern matching
--
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
--
printResult' :: Either LoginError String -> IO ()
printResult' email =
    putStrLn $ either (const "ERROR: Invalid e-mail") ("Address: " ++) email

-----------------------------------------------------------------------

main :: IO ()
main = do
  printResult  $ checkEmail "rey@skywalker.com"
  printResult' $ checkEmail "kylo@ren@solo.org"

