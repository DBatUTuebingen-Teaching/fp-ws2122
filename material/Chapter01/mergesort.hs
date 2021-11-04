-- Mergesort list xs with respect to ordering <<<

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort (<<<) []  = []
mergeSort (<<<) [x] = [x]
mergeSort (<<<) xs  = merge (<<<) (mergeSort (<<<) ls) (mergeSort (<<<) rs)
  where
    (ls, rs) = splitAt (length xs `div` 2) xs

    merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    merge (<<<) []        ys        = ys
    merge (<<<) xs        []        = xs
    merge (<<<) l1@(x:xs) l2@(y:ys)
      | x <<< y   = x:merge (<<<) xs l2
      | otherwise = y:merge (<<<) l1 ys

main :: IO ()
main = print $ mergeSort (>) [1,3..19]
