import Data.List.Split (splitOn)
import Data.Map as Map


-- The combined (stacked) monad:
--
-- A computation that performs I/O and then either
-- - fails with an exception (of type 𝘦) or
-- - succeeds and returns with a result (of type 𝘢):
--
newtype EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
  fmap f v = pure f <*> v

-- Verify that the above does the "right thing".
-- The following is equivalent:
--
-- (1) unwrap, (2) apply f inside IO + Either, (3) wrap again
--
-- instance Functor (EitherIO e) where
--     fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
    pure x  = return x
    u <*> v = u >>= \f -> v >>= \x -> return (f x)

instance Monad (EitherIO e) where
  return x = EitherIO $ (return . Right) x
  x >>= f  = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)


-- How to lift values into the EitherIO monad
--
liftEither :: Either e a -> EitherIO e a
liftEither x =
    EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x =
    EitherIO (fmap Right x) -- recall: IO is a functor (we may use fmap)

-- Throwing and catching exceptions
--
throw :: 𝘦 -> EitherIO 𝘦 𝘢
throw exc = liftEither (Left exc)

catch :: EitherIO 𝘦 𝘢 -> (𝘦 -> EitherIO 𝘦 𝘢) -> EitherIO 𝘦 𝘢
catch comp handler = EitherIO $ do                -- ⚠️ Run the actions in the regular I/O monad
  result <- runEitherIO comp                      -- because we want to detect and process
  case result of                                  -- exceptions ourselves (running in EitherIO
    Left exc -> runEitherIO (handler exc)         -- instead would handle exceptions by stopping
    success  -> return success                    -- the computation immediately).

-----------------------------------------------------------------------

-- Several kinds of execptions can occur during login
--
data LoginError = InvalidEmail | NoSuchUser | WrongPassword
  deriving Show

-- Verify that e-mail address contains exactly one '@'
--
checkEmail :: String -> Either LoginError String
checkEmail email = case splitOn "@" email of
                     [_name, domain] -> Right email
                     _               -> Left InvalidEmail

-----------------------------------------------------------------------

-- ➊ Prompt and read e-mail address from standard input,
-- ➋ then verify it
--
askForEmail :: EitherIO LoginError String
askForEmail = do
    liftIO $ putStrLn "Enter e-mail address:"
    email <- liftIO getLine
    liftEither $ checkEmail email

-----------------------------------------------------------------------

-- "Database" of known users and their clear-text passwords :-/
--
users :: Map String String
users = Map.fromList
    [ ("rey@skywalker.com", "grandpa")
    , ("root@localhost",    "000000")
    ]

-- Perform user login, return e-mail address on success
--
--  ➊ Interactively ask for e-mail address
--  ➋ Check for user in user database
--  ➌ Interactively read password
--  ➍ Compare database password and given password
--
userLogin :: EitherIO LoginError String
userLogin = do
  email    <- askForEmail
  userpw   <- case Map.lookup email users of
                Just pw -> return pw
                Nothing -> throw NoSuchUser
  password <- liftIO $ do putStrLn "Enter password:"
                          getLine
  if userpw == password then
    return email
  else
    throw WrongPassword

-----------------------------------------------------------------------

-- Exception handlers

-- On incorrect password, offer a second attempt to enter it,
-- otherwise re-raise the exception (i.e., we do not (know how to) handle it)
--
wrongPassword :: LoginError -> EitherIO LoginError String
wrongPassword WrongPassword = do
  liftIO $ putStrLn "Wrong password, try once more."
  userLogin
wrongPassword exc = throw exc


-- Non-recoverable failure.  Print error message and re-throw.
abortLogin :: LoginError -> EitherIO LoginError a
abortLogin exc = do
  liftIO $ putStrLn $ case exc of
                        InvalidEmail  -> "Invalid e-mail address entered."
                        NoSuchUser    -> "No user with that e-mail exists."
                        WrongPassword -> "Second wrong password, abort."
  throw exc

-----------------------------------------------------------------------

-- Main user login dialogue, offering a 2nd chance after a
-- failed password attempt:
--
--  ➊ Attempt user login.
--              ⎧ Yes, logged in.
--  ➋ Success?  ⎨                ⎧ Yes, offer 2nd attempt. Goto ➊.
--              ⎩ First failure? ⎨
--                               ⎩ No, abort. Print error message.
--
loginDialogue :: EitherIO LoginError String
loginDialogue = do
  email <- userLogin `catch` wrongPassword `catch` abortLogin
  liftIO $ putStrLn ("Logged in as " ++ email)
  return email


main :: IO ()
main = do
  email <- runEitherIO loginDialogue
  print email
