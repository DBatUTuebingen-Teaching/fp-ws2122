-- A sum type (enumeration)
-- whose values can be shown and compared for equality

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq)

-- Is this day on a weekend?
weekend :: Weekday -> Bool
weekend Sat = True
weekend Sun = True
weekend _   = False

main :: IO ()
main = print (weekend Thu, weekend Sat)
