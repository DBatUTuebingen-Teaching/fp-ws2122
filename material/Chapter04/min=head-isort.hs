import Prelude hiding (min)

-- A crazy(?) but declarative definition of list minimum:
-- sort the list in ascending order, then take the first element

min :: Ord a => [a] -> a
min = head . isort                                         -- [min]

isort :: Ord a => [a] -> [a]
isort []     = []                                      -- [isort.1]
isort (x:xs) = ins x (isort xs)                        -- [isort.2]
  where
    ins x []                 = [x]                       -- [ins.1]
    ins x (y:ys) | x < y     = x:y:ys                    -- [ins.2]
                 | otherwise = y:ins x ys                -- [ins.3]

main :: IO ()
main = do
  print $ isort [8,6,1,7,5]
  print $ min [8,6,1,7,5]

-- Hint: Use  :set +s  in GHCi to activate timing for expression evaluation,
--       you will find that min uses O(n) to find the list's minimum value.
