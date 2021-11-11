-- A product type (single constructor)

-- Sequences are lists whose length is maintained explicitly:
--                      list
--               length  |
--                   |   |
data Sequence a = S Int [a]
  deriving (Eq, Show)

-- Build a sequence from vanilla Haskell list xs
fromList :: [a] -> Sequence a
fromList xs = S (length xs) xs

-- Concatenate two sequences
(+++) :: Sequence a -> Sequence a -> Sequence a
S lx xs +++ S ly ys = S (lx + ly) (xs ++ ys)

-- Length of a sequence in O(1)
len :: Sequence a -> Int
len (S l _) = l

main :: IO ()
main = print $ len (fromList ['a'..'m'] +++ fromList ['n'..'z'])
