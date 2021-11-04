-- Sum of all elements in xs (using guards, head/tail)
sum' :: [Integer] -> Integer
sum' xs | xs == []  = 0
        | otherwise = head xs + sum' (tail xs)

-- Sum of all elements in xs (using pattern matching)
sum'' :: [Integer] -> Integer
sum'' []     = 0
sum'' (x:xs) = x + sum'' xs


main :: IO ()
main = print $ (sum' [1..100], sum'' [1..100])
