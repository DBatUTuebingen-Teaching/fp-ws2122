import Prelude hiding(min, (<=), (>=), (/=))
import Data.List (partition)
import GHC.Integer

class Comparable a where
  (<=) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

class Printable a where
  toString :: a -> String
  width :: a -> Int
  width = maximum . map length . lines . toString

instance Comparable Integer where
  (<=) = ltInteger
  (>=) = geInteger
  (/=) = neqInteger

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Printable Weekday where
  toString Mon = "Monday"
  toString Tue = "Tuesday"
  toString Wed = "Wednesday"
  toString Thu = "Thursday"
  toString Fri = "Friday"
  toString Sat = "Saturday"
  toString Sun = "Sunday"

instance Comparable Weekday where
  a <= b = ltInteger (index a) (index b)
    where
      index :: Weekday -> Integer
      index Mon = 1
      index Tue = 2
      index Wed = 3
      index Thu = 4
      index Fri = 5
      index Sat = 6
      index Sun = 7

  a /= b = not $ a <= b && b <= a
  a >= b = not $ a <= b

table :: (Comparable a, Printable a) => [a] -> String
table a = decoration ++ "\n" ++ concatMap ((++ "\n") . toString) a' ++ decoration
  where
    a' = qsort a
    w = maximum $ map width a
    decoration = replicate w '='

qsort :: Comparable a => [a] -> [a]
qsort [] = []
qsort (q:qs) = qsort l ++ [q] ++ qsort g
  where
    (l, g) = partition (\ e -> e <= q) qs
