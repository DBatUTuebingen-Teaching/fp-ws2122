take' :: Integer -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

main :: IO ()
main = print $ take' 20 [1,3..]
