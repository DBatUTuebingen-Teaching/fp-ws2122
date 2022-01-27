-- Based on
-- http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/


-- Constructors for IO DSL

-- Possible IO actions:
--   Return x: no side effect, just return x (lifting)
--   Put s io: print string s to stdout, then perform IO action io
--   Get io:   read string from stdin, then pass string to IO action io

data IOaction a =
    Return a
  | Put String (IOaction a)
  | Get (String -> IOaction a)

-- Show an IO action
-- (NB: the function in a Get IO action is invoked with a unique variable ($n)
--  in place of the string it will at runtime)

instance Show a => Show (IOaction a) where
  show io = pr 0 0 io
    where
      pr :: Show a => Int -> Int -> IOaction a -> String
      pr m var (Return a) = ind m "Return " ++ show a
      pr m var (Put s io) = ind m "Put " ++ show s ++ " (\n" ++
                            pr (m + 2) var io ++ "\n" ++
                            ind m ")"
      pr m var (Get io)   = let v = "$" ++ show var
                            in ind m "Get (\\" ++ v ++ " -> \n" ++
                               pr (m + 2) (var + 1) (io v) ++ "\n" ++
                               ind m ")"

      ind m s = replicate m ' ' ++ s


-- Two simple composite IO actions
put :: String -> IOaction ()
put s = Put s (Return ())

get :: IOaction String
get = Get (\s -> Return s)


-- Sequentially perform two IO actions (bind)
seqio :: IOaction a -> (a -> IOaction b) -> IOaction b
seqio (Return a) g = g a
seqio (Put s io) g = Put s (seqio io g)
seqio (Get io)   g = Get (\s -> seqio (io s) g)

-- A third composite IO action,
-- the equivalent of Get (\s -> Put s (Return ()))
echo :: IOaction ()
echo = get `seqio` put

-- A complex IO action
hithere :: IOaction ()
hithere = put "What is your name?"      `seqio` \_    ->
          get                           `seqio` \name ->
          put "What is your age?"       `seqio` \_    ->
          get                           `seqio` \age  ->
          put ("Hello " ++ name ++ "!") `seqio` \_    ->
          put ("You are " ++ age ++ " years old")



-----------------------------------------------------------------------
-- A monad instance for IO action

instance Functor IOaction where
  fmap f v = pure f <*> v

instance Applicative IOaction where
  pure x  = return x
  u <*> v = u >>= \f -> v >>= \x -> return (f x)

instance Monad IOaction where
  return = Return
  (>>=)  = seqio

-- Using do notation to equivalently rephrase hithere
hithere' :: IOaction ()
hithere' = do put "What is your name?"
              name <- get
              put "What is your age?"
              age <- get
              put ("Hello, " ++ name ++ "!")
              put ("You are " ++ age ++ " years old!")

-----------------------------------------------------------------------
-- Observer for the IO DSL: interpret the IO actions in Haskell's IO monad

runIO :: IOaction a -> IO a
runIO (Return a) = return a
runIO (Put s io) = putStrLn s >> runIO io
runIO (Get io)   = getLine >>= \s -> runIO (io s)


main :: IO ()
main = do
  print hithere

  div

  print hithere'

  div

  print $ do hithere
             echo
  div

  runIO $ do hithere
             echo

  where
    div = putStrLn "--- ✄ --- ✄ --- ✄ --- ✄ --- ✄ ---"


