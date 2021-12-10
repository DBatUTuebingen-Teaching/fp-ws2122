module PatternMatching (Pattern,
                        lit, empty, fail,
                        alt, seq, rep, rep1,
                        alts, seqs, lits, oneof, app) where

import Prelude hiding (seq, fail)

-- Given a string, a pattern returns the (possibly empty) list of
-- possible matches.  A match consists of a match value (e.g., matched
-- the matched character(s), token, or parse tree) and the residual string
-- left to match.
--
--    ♫♩ "A pattern for things
--        is a function from strings
--        to lists of things and strings."

type Pattern a = String -> [(a, String)]

-- BASIC PATTERNS

-- match character c, returning the matched character
lit ::  Char -> Pattern Char
lit _ []                  = []
lit c  (x:xs) | c == x    = [(c, xs)]
              | otherwise = []

-- match the empty string, return v
empty :: a -> Pattern a
empty v xs = [(v, xs)]

-- fail always (yields empty list of matches)
fail :: Pattern a
fail _ = []


-- COMBINE PATTERNS

-- match p or q
alt :: Pattern a -> Pattern a -> Pattern a
alt p q xs = p xs ++ q xs

-- match p and q in sequence (use f to combine match values)
seq :: (a -> b -> c) -> Pattern a -> Pattern b -> Pattern c
seq f p q xs = [ (f v1 v2, xs2) | (v1, xs1) <- p xs, (v2, xs2) <- q xs1 ]

-- Equivalent to the above:
-- seq f p q xs = concat (map (\(v1, xs1) ->
--                          map (\(v2, xs2) -> (f v1 v2, xs2))
--                              (q xs1))
--                          (p xs))



-- match p repeatedly (including 0 times)
-- rep (lit c) matches "", "c", "cc", "ccc", ...
rep :: Pattern a -> Pattern [a]
rep p = alt (seq (:) p (rep p))
            (empty [])

-- match p repeatedly, but at least once
rep1 :: Pattern a -> Pattern [a]
rep1 p = seq (:) p (rep p)



-- CONVENIENCE

-- build choice pattern from a list of alternatives
-- alts [p₁,p₂,p₃] = alt p₁ (alt p₂ (alt p₃ fail))
alts :: [Pattern a] -> Pattern a
alts = foldr alt fail

-- build sequence pattern from a list of patterns
-- seqs [p₁,p₂,p₃] = seq (:) p₁ (seq (:) p₂ (seq (:) p₃ (empty [])))
seqs :: [Pattern a] -> Pattern [a]
seqs = foldr (seq (:)) (empty [])

-- match a string cs (= sequence of characters)
lits :: String -> Pattern String
lits cs = seqs [ lit c | c <- cs ]

-- match any character in string cs
oneof :: String -> Pattern Char
oneof cs = alts [ lit c | c <- cs ]

-- apply function f to match value (for match post-processing)
app :: (a -> b) -> Pattern a -> Pattern b
app f p xs = [ (f v1, xs1) | (v1, xs1) <- p xs ]
