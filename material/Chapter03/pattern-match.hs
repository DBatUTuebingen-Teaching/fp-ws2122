import Prelude hiding (seq,fail)
import PatternMatching


-- fortytwo ≡ '4' '2'
fortytwo :: Pattern (Char,Char)
fortytwo = seq (,) (lit '4') (lit '2')

-- digit ≡ ['0'-'9']
digit :: Pattern Char
digit = lit '0' `alt` lit '1' `alt` lit '2' `alt` lit '3' `alt`
        lit '4' `alt` lit '5' `alt` lit '6' `alt` lit '7' `alt`
        lit '8' `alt` lit '9'

-- Equivalent formulation using the conveniene functions in
-- module PatternMatching:  digit = oneof ['0'..'9']


-- number ≡ digit*
number :: Pattern String
number = rep digit


-- smiley ≡ [:;]-[()]
smiley :: Pattern String
smiley = seq (:) (alt (lit ':') (lit ';'))
                 (seq (:) (lit '-')
                          (seq (:) (alt (lit '(') (lit ')'))
                                   (empty [])))

-- Equivalent formulation using the conveniene functions in
-- module PatternMatching:  smiley = seqs [oneof ":;", lit '-', oneof "()"]

-----------------------------------------------------------------------

main :: IO ()
main = do
  print $ fortytwo "42 forever"
  print $ rep digit "42 forever"
  print $ smiley ";-)"
