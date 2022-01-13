-- Demonstrate modular program construction through laziness:
-- value generation (subdivide) and consume/test (within)
-- can be implemented separately.
--
-- See John Hughes, "Why Functional Programming Matters", Section 4.3

import Prelude hiding (zipWith)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- Consume list until two adjacent elements are
-- 1. within eps of each other
-- 2. differ by a factor less than eps
within :: (Ord a, Num a) => a -> [a] -> a
within eps (x1:x2:xs) | abs (x1 - x2) <= eps = x2
                      | otherwise            = within eps (x2:xs)

relative :: (Ord a, Fractional a) => a -> [a] -> a
relative eps (x1:x2:xs) | abs (x1/x2 - 1) <= eps = x2
                        | otherwise              = relative eps (x2:xs)


-- The area below f between x1 and x2, UNDER THE ASSUMPTION that
-- f describes a straight line between points (x1, f x1) and (x2, f x2)
interpolate :: Floating a => (a -> a) -> a -> a -> a
interpolate f x1 x2 = (f x1 + f x2) * (x2 - x1) / 2

-- Approximations of the integral of f between x1 and x2 by dividing
-- the [x1,x2] interval in half repeatedly
subdivide :: Floating a => (a -> a) -> a -> a -> [a]
subdivide f x1 x2 = interpolate f x1 x2 :
                      zipWith (+) (subdivide f x1 mid) (subdivide f mid x2)
  where
    mid = (x1 + x2) / 2

-- Integrate f in the area between x1 and x2
integrate :: (Ord a, Floating a) => a -> (a -> a) -> a -> a -> a
integrate eps f x1 x2 = within eps (subdivide f x1 x2)

main :: IO ()
main = print $ integrate 0.0001 cos 0 (pi/2)   -- exact result: 1.0
