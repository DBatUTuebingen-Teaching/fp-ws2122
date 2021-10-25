-- The list of factors of n
factors :: Integer -> [Integer]
factors n = [ m | m <- [2..n-1], mod n m == 0 ]

-- To see the effects of lazy evaluation in ghci:
--
-- 1.  fs = factors 55555
-- 2.  :sprint fs
-- 3.  fs == []
-- 4.  :sprint fs
