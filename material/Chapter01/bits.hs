type Bits = [Integer]    -- crude type :-(, we expect lists of 0,1 only

type Predicate a = a -> Bool

-- Bit list for n (least significant bit first)
bits :: Integer -> Bits
bits n | n == 0    = [0]
       | otherwise = (n `mod` 2) : bits (n `div` 2)

-- Is n even?
-- (Efficient? Yes, only the LSB of n is actually computed.)
isEven :: Predicate Integer
isEven n = head (bits n) == 0

main :: IO ()
main = print $ isEven 35
