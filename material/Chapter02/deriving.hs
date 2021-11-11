-- Demonstrate algebraic data types: sum type

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Show, Ord, Enum, Bounded)

-- Is this day on a weekend?
weekend :: Weekday -> Bool
weekend Sat = True
weekend Sun = True
weekend _   = False

-- The classic rock/paper/scissor game

data Move = Rock | Paper | Scissor
  deriving (Eq)

data Outcome = Lose | Tie | Win
  deriving (Show)

-- Outcome of a game round (us vs. them)
outcome :: Move -> Move -> Outcome
outcome Rock    Scissor = Win
outcome Paper   Rock    = Win
outcome Scissor Paper   = Win
outcome us      them
  | us == them = Tie
  | otherwise  = Lose

main :: IO ()
main = do
  print $ Thu < Sat
  print   [Tue .. Thu]
  print   [Fri,Thu .. Mon]
  print $ outcome Paper Paper
