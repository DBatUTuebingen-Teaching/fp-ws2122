import Data.List.Split (splitOn)
import Data.Map as Map


-- A combination (stack) of monads m and Either.
--
-- Run a computation in monad m and then either
-- - fail with an exception (of type 𝘦) or
-- - succeed and return with a result (of type 𝘢):
--
newtype EitherT e m a = EitherT {
    runEitherT :: m (Either e a)
}

instance Monad m => Functor (EitherT e m) where
  fmap f v = pure f <*> v

instance Monad m => Applicative (EitherT e m) where
    pure x  = return x
    u <*> v = u >>= \f -> v >>= \x -> return (f x)

instance Monad m => Monad (EitherT e m) where
  return x = EitherT $ (return . Right) x
  x >>= f  = EitherT $ runEitherT x >>= either (return . Left) (runEitherT . f)

-- How to lift values into the stacked monad
--
liftEither :: Monad m => Either e a -> EitherT e m a
liftEither x =
    EitherT (return x)

lift :: Functor m => m a -> EitherT e m a
lift x =
    EitherT (fmap Right x)

-- Throwing and catching exceptions
--
throw :: Monad m => 𝘦 -> EitherT 𝘦 m 𝘢
throw exc = liftEither (Left exc)

catch :: Monad m => EitherT 𝘦 m 𝘢 -> (𝘦 -> EitherT 𝘦 m 𝘢) -> EitherT 𝘦 m 𝘢
catch comp handler = EitherT $ do                -- ⚠️ Run the actions in the regular monad m
  result <- runEitherT comp                      -- because we want to detect and process
  case result of                                 -- exceptions ourselves (running in EitherT
    Left exc -> runEitherT (handler exc)         -- instead would handle exceptions by stopping
    success  -> return success                   -- the computation immediately).

-----------------------------------------------------------------------

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

-----------------------------------------------------------------------

-- ➊ Prompt and read e-mail address from standard input,
-- ➋ then verify it
--
askForEmail :: EitherT LoginError IO String
askForEmail = do
    lift $ putStrLn "Enter e-mail address:"
    email <- lift getLine
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
userLogin :: EitherT LoginError IO String
userLogin = do
  email    <- askForEmail
  userpw   <- case Map.lookup email users of
                Just pw -> return pw
                Nothing -> throw NoSuchUser
  password <- lift $ do putStrLn "Enter password:"
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
wrongPassword :: LoginError -> EitherT LoginError IO String
wrongPassword WrongPassword = do
  lift $ putStrLn "Wrong password, try once more."
  userLogin
wrongPassword exc = throw exc


-- Non-recoverable failure.  Print error message and re-throw.
abortLogin :: LoginError -> EitherT LoginError IO a
abortLogin exc = do
  lift $ putStrLn $ case exc of
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
loginDialogue :: EitherT LoginError IO String
loginDialogue = do
  email <- userLogin `catch` wrongPassword `catch` abortLogin
  lift $ putStrLn ("Logged in as " ++ email)
  return email


main :: IO ()
main = do
  email <- runEitherT loginDialogue
  print email
