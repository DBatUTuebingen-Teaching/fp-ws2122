
type NonDet a = [a]

(>=>) :: (a -> NonDet b) -> (b -> NonDet c) -> (a -> NonDet c)
f >=> g = \x -> concat [ g y | y <- f x ]


oneOf :: [a] -> NonDet a
oneOf xs = xs

culDeSac :: NonDet a
culDeSac = []


multiplyTo :: Integer -> NonDet (Integer, Integer)
multiplyTo m = (const (oneOf [1..m]) >=>                        -- compute Cartesian product
                 (\x -> [ (x,y) | y <- oneOf [1..m] ]) >=>      -- [1..m] × [1..m]
                   \(x,y) -> if x * y == m then [(x,y)] else culDeSac) $ m

main :: IO ()
main = print $ multiplyTo 42

