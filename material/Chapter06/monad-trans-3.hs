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
                Nothing -> liftEither (Left NoSuchUser)
  password <- liftIO $ do putStrLn "Enter password:"
                          getLine
  if userpw == password then
    return email
  else
    liftEither (Left WrongPassword)

-----------------------------------------------------------------------

main :: IO ()
main = do
  email <- runEitherIO askForEmail
  printResult email

  email <- runEitherIO userLogin
  printResult email
