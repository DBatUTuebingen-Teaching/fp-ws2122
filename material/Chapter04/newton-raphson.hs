-- Demonstrate modular program construction through laziness:
-- value generation (iterate) and consume/test (within)
-- can be implemented separately.
--
-- Can replace test (within → relative) without modifying the generator.
--
-- See John Hughes, "Why Functional Programming Matters", Section 4.1


import Prelude hiding (iterate, sqrt)

-- [x, f x, f (f x), f (f (f x)), ...
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- Consume infinite list until two adjacent elements are
-- 1. within eps of each other
-- 2. differ by a factor less than eps
within :: (Ord a, Num a) => a -> [a] -> a
within eps (x1:x2:xs) | abs (x1 - x2) <= eps = x2
                      | otherwise            = within eps (x2:xs)


relative :: (Ord a, Fractional a) => a -> [a] -> a
relative eps (x1:x2:xs) | abs (x1/x2 - 1) <= eps = x2
                        | otherwise              = relative eps (x2:xs)

-- Square root of x using the Newton-Raphson algorithm:
--
--   a₀ = x / 2
--   aᵢ = (aᵢ₋₁ + x / aᵢ₋₁) / 2
--
-- Why does this work?  If the approximations aᵢ converge to some
-- limit a, then:
--
--   a = (a + x / a) / 2
--  2a = a + x / a
--   a = x / a
--  a² = x
--   a = √x

sqroot :: Double -> Double -> Double
sqroot eps x = relative eps (iterate next a0)
--                ↑
--          or: relative
  where
    -- initial approximation
    a0 :: Double
    a0 = x / 2
    -- find next aᵢ₊₁, given aᵢ
    next :: Double -> Double
    next a = (a + x / a) / 2

sqrt :: Double -> Double
sqrt = sqroot 0.001

main :: IO ()
main = print $ sqrt 81
