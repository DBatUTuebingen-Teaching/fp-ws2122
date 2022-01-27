import Data.List (nub)

-- A non-deterministic value may take on any value in the given list
newtype NonDet a = ND { unND :: [a] }
  deriving (Show)

-- NB: The use of newtype could be replaced by a single-constructor
--     algebraic data type:
--
-- data NonDet a = ND [a]
--   deriving (Show)

-- unND :: NonDet a -> [a]
-- unND (ND xs) = xs


-- Instantiate the Functor-Applicative-Monad tower:

instance Functor NonDet where
  fmap f v = pure f <*> v

  -- expand/simplify the above to find:
  --
  -- fmap f (ND v) = ND $ map f v

instance Applicative NonDet where
  pure x = return x

  -- expand/simplify the above to find:
  --
  -- pure x = ND [x]

  u <*> v = u >>= \f -> v >>= \x -> return (f x)

  -- expand/simplify the above to find the already
  -- well-known definition of <*> for lists:
  -- (ND fs) <*> (ND xs) = ND $ [ f x | f <- fs, x <- xs ]


instance Monad NonDet where
  return x = ND [x]

  -- xs represents all possible results of prior computation: apply
  -- non-deterministic computation g to all possible values x in xs
  -- and collect overall results:
  (ND xs) >>= g = ND $ concat [ unND (g x) | x <- xs ]


-- Non-deterministically pick one value out of a list
oneOf :: [a] -> NonDet a
oneOf xs = ND xs

-- Outcome of a failed non-deterministic computation
failure :: NonDet a
failure = ND []



-- Examples

-- Find all pairs of integers (x,y) such that x * y == n
--
multiplyTo :: Integer -> NonDet (Integer, Integer)
multiplyTo n = oneOf [1..n] >>= \x ->
               oneOf [x..n] >>= \y ->
               if x * y == n then return (x,y) else failure

-- Solve the digit assignment (verbal arithmetic, cryptarithm) puzzle
--
--    SEND
-- +  MORE
-- -------
-- = MONEY
--
-- Note:
--   - Assigned digits must be unique
--   - M obviously must be 1
--
sendMoreMoney :: NonDet (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
sendMoreMoney = oneOf [1..9] >>= \s ->
                oneOf [0..9] >>= \e ->
                oneOf [0..9] >>= \n ->
                oneOf [0..9] >>= \d ->
                oneOf [1]    >>= \m ->
                oneOf [0..9] >>= \o ->
                oneOf [0..9] >>= \r ->
                oneOf [0..9] >>= \y ->
                if allDifferent [s,e,n,d,m,o,r,y]
                   &&
                   val [s,e,n,d] + val [m,o,r,e] == val [m,o,n,e,y]
                then return (s,e,n,d,m,o,r,y)
                else failure
  where
    allDifferent :: Eq a => [a] -> Bool
    allDifferent xs = length xs == length (nub xs)

    val :: Num a => [a] -> a
    val = foldl (\x y -> x * 10 + y) 0


main :: IO ()
main = do
  print $ multiplyTo 42
  print $ unND sendMoreMoney
