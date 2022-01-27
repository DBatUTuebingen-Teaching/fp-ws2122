import Data.List.Split (splitOn)
import Data.Map as Map

-- Several kinds of execptions can occur during login
--
data LoginError = InvalidEmail | NoSuchUser | WrongPassword
  deriving Show

-- Verify that e-mail address contains exactly one '@'
--
checkEmail :: String -> Either LoginError String
checkEmail email = case splitOn "@" email of
                     [_name, _domain] -> Right email
                     _                -> Left InvalidEmail


-- Print result returned by exception-generating computation
--
printResult :: Either LoginError String -> IO ()
printResult email =
  putStrLn $ case email of
               Right addr         -> "Address: " ++ addr
               Left InvalidEmail  -> "Invalid e-mail address entered."
               Left NoSuchUser    -> "No user with that e-mail exists."
               Left WrongPassword -> "Wrong password."

-----------------------------------------------------------------------
-- Still OK: perform all I/O, only then check the e-mail address

-- ➊ Prompt and read e-mail address from standard input,
-- ➋ then verify it
--
askForEmail :: IO (Either LoginError String)
askForEmail = do
    putStrLn "Enter e-mail address:"  --    ⎤
    email <- getLine                  --   ➊⎥
    return (checkEmail email)         -- ➋] ⎦

-----------------------------------------------------------------------
-- Messy: exception handling/generation and I/O mix

-- "Database" of known users and their clear-text passwords :-/
--
users :: Map String String
users = Map.fromList
    [ ("rey@skywalker.com", "grandpa")
    , ("root@localhost",    "000000")
    ]

-- Perform user login, return e-mail address on success
--
--  ➊ I/O: interactively ask for e-mail address
--  ➋ Exception handling: e-mail address OK?
--  ➌ If so, I/O: interactively ask for and check password
--  ➍ If failed, exception generation: invalid password
--
--  This mix leads to an ugly nesting of I/O and exception handling:
--
userLogin :: IO (Either LoginError String)
userLogin = do
  email <- askForEmail                                  -- ───────────┐
  case email of                                         -- ────────┐  │
    Right addr -> case Map.lookup addr users of         --         │  │
                    Just userpw -> do                   -- ─────┐  │  │
                      putStrLn "Enter password:"        --      │  │  │
                      password <- getLine               --      │  │  │
                      if userpw == password then        --     ➌│ ➋│ ➊│
                        return email                    -- ─┐   │  │  │
                      else                              -- ➍│   │  │  │
                        return (Left WrongPassword)     -- ─┘───┘  │  │
                    Nothing -> return (Left NoSuchUser) -- ────────┘  │
    exc@(Left _) -> return exc                          -- ───────────┘

-----------------------------------------------------------------------

main :: IO ()
main = do
  email <- askForEmail
  printResult email

  email <- userLogin
  printResult email
