import Data.Maybe (fromMaybe)
import Data.Char (ord)

-- Create a new data type that we can make an instance of
-- Monad (and Applicative, Functor)
--
-- State transformer (a function of type a -> ST b consumes argument
-- of type a and the current state to yield a result of type b and
-- and a following state):
data ST b = ST (State -> (State, b))

type State = String


-- Instantiate the Functor-Applicative-Monad tower:

instance Functor ST where
  fmap f v = pure f <*> v

  -- expand/simplify the above to find:
  --
  -- fmap f (ST v) = ST $ \s0 -> let (s1, x) = v s0 in
  --                             (s1, f x)

instance Applicative ST where
  pure x = return x

  -- expand/simplify the above to find:
  --
  -- pure x = ST $ \s -> (s, x)

  u <*> v = u >>= \f -> v >>= \x -> return (f x)

  -- expand/simplify the above to find:
  --
  -- (ST u) <*> (ST v) = ST $ \s0 -> let (s1, f) = u s0 in
  --                                 let (s2, x) = v s1 in
  --                                 (s2, f x)

instance Monad ST where
  return x = ST $ \s -> (s, x)

  (ST f) >>= g = ST $ \s0 -> let (s1, y) = f s0 in
                             let (ST h)  = g y  in
                             h s1


numeralToDigit :: String -> ST Char
numeralToDigit w = ST $ \s -> (s ++ "numeralToDigit ", fromMaybe '0' (lookup w digits))
  where
    digits = [("null",  '0'),
              ("zero",  '0'),
              ("one",   '1'),
              ("two",   '2'),
              ("three", '3'),
              ("four",  '4'),
              ("five",  '5'),
              ("six",   '6'),
              ("seven", '7'),
              ("eight", '8'),
              ("nine",  '9')]

digitToVal :: Char -> ST Int
digitToVal d | d `elem` ['0'..'9'] = ST $ \s -> (s ++ "digitToVal ", ord d - ord '0')

chineseNumeral :: Int -> ST Char
chineseNumeral n = ST $ \s -> (s ++ "chineseNumeral ", "零一二三四五六七八九" !! n)


-- Reformulation of the English numeral to Chinese digit conversion
chinese' :: String -> ST Char
chinese' n = numeralToDigit n >>= digitToVal >>= chineseNumeral


-- Convenience: Run a stateful computation
runST :: ST a -> a
runST (ST f) = snd $ f ""


main :: IO ()
main = do
  putStrLn $ [runST $ chinese' "five"]
  -- also show final state:
  print $ case chinese' "five" of ST f -> f ""
