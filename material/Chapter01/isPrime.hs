-- Is n a prime number?
isPrime :: Integer -> Bool
isPrime n = factors n == []
  where
    factors :: Integer -> [Integer]
    factors n = [ m | m <- [2..n-1], mod n m == 0 ]


main :: IO ()
main = do
  let n = 43
  print (isPrime n)
