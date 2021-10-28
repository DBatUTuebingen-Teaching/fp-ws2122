-- Numbers closer than this are considered equal
epsilon :: Double
epsilon = 1.0e-4


-- User-defined operator: approximate equality of x and y
(~=~) :: Double -> Double -> Bool
x ~=~ y = abs (x - y) < epsilon

{-
  Declare the usual comparison operator precedence,
  no (left/right) associativity: e1 ~=~ e2 ~=~ e3 is a syntactic error
-}
infix 4 ~=~


main :: IO ()
main = print $ cos (0.5 * pi) ~=~ 0.0
