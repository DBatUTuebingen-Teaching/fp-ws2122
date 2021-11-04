
-- (1) Layout:

power :: Double -> Integer -> Double
power x k | k == 1    = x
         | even k    = power (x * x) (halve k)
         | otherwise = x * power (x * x) (halve k)
 where
     even n  = n `mod` 2 == 0
     halve n = n `div` 2

main :: IO ()
main = print $ power 2 16

{-

-- (2) Equivalent explicit syntax:

{power :: Double -> Integer -> Double
;power x k | k == 1    = x
           | even k    = power (x * x) (halve k)
           | otherwise = x * power (x * x) (halve k)
   where
     {even n  = n `mod` 2 == 0
     ;halve n = n `div` 2}

;main :: IO ()
;main = print $ power 2 16}

-}

{-

-- (3) A valid Haskell program (*yuck*...):

{
 power :: Double -> Integer -> Double;
 power x k | k == 1    = x
           | even k    = power (x * x) (halve k)
           | otherwise = x * power (x * x) (halve k)
   where {
     even n  = n `mod` 2 == 0;
     halve n = n `div` 2
   };

 main :: IO ();
 main = print $ power 2 16
}

-}
