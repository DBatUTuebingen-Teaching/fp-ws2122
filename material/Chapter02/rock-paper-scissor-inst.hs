-- Manually define type class instances for algebraic data types Outcome
-- and Move.  This leads to tedious boilerplate work.


import Data.Maybe
import Data.Tuple

-- The classic rock/paper/scissor game.

data Outcome = Lose | Tie | Win

instance Eq Outcome where
  Lose == Lose = True
  Tie  == Tie  = True
  Win  == Win  = True
  _    == _    = False

instance Enum Outcome where
  fromEnum Lose = 0
  fromEnum Tie  = 1
  fromEnum Win  = 2

  toEnum 0 = Lose
  toEnum 1 = Tie
  toEnum 2 = Win

instance Show Outcome where
  show Lose = "Lose"
  show Tie  = "Tie"
  show Win  = "Win"

instance Ord Outcome where
  Lose <= Lose = True
  Lose <= Tie  = True
  Lose <= Win  = True
  Tie  <= Tie  = True
  Tie  <= Win  = True
  Win  <= Win  = True
  _    <= _    = False

instance Bounded Outcome where
  minBound = Lose
  maxBound = Win

-----------------------------------------------------------------------------


data Move = Rock | Paper | Scissor

instance Eq Move where
  Rock    == Rock    = True
  Paper   == Paper   = True
  Scissor == Scissor = True
  _       == _       = False

-- Lookup table defining a consistent mapping between Move and Int
table :: [(Move, Int)]
table = [(Rock, 0), (Paper, 1), (Scissor, 2)]

instance Enum Move where
  fromEnum m = fromJust $ lookup m table
  toEnum n   = fromJust $ lookup n $ map swap table

instance Show Move where
  show Rock    = "Rock"
  show Paper   = "Paper"
  show Scissor = "Scissor"

instance Ord Move where
  Rock    <= Rock    = True
  Rock    <= Paper   = True
  Rock    <= Scissor = True
  Paper   <= Paper   = True
  Paper   <= Scissor = True
  Scissor <= Scissor = True
  _       <= _       = False

instance Bounded Move where
  minBound = Rock
  maxBound = Scissor


-----------------------------------------------------------------------------


outcome :: Move -> Move -> Outcome
outcome Rock    Scissor = Win
outcome Paper   Rock    = Win
outcome Scissor Paper   = Win
outcome us      them
  | us == them = Tie
  | otherwise  = Lose

main :: IO ()
main = print $ outcome Paper Paper
