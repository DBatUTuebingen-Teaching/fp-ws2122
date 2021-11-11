-- Our own formulation of cons lists
data List a = Nil
            | Cons a (List a)
  deriving (Show)

-- Haskell's builtin type [a] and List a are isomorphic:
--         toList . fromList = id :: List a -> List a
--   and   fromList . toList = id :: [a] -> [a]
toList :: [a] -> List a
toList []     = Nil
toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Nil         = []
fromList (Cons x xs) = x:fromList xs


-- The family of well-known list functions (combinators) can be
-- reformulated for List a:
mapList :: (a -> b) -> List a -> List b
mapList _ Nil         = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

filterList :: (a -> Bool) -> List a -> List a
filterList _ Nil                     = Nil
filterList p (Cons x xs) | p x       = Cons x (filterList p xs)
                         | otherwise = filterList p xs

-- Lift function f over Haskell lists to a function over our own lists
liftList :: ([a] -> [b]) -> List a -> List b
liftList f = toList . f . fromList

mapList' :: (a -> b) -> List a -> List b
mapList' f = liftList (map f)

filterList' :: (a -> Bool) -> List a -> List a
filterList' p = liftList (filter p)


main :: IO ()
main = do
  print $ fromList (toList [1..5])
  print $ mapList' odd $ toList [1..5]
