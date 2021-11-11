-- A dictionary that maps a to b values
type Dictionary a b = [(a,b)]

type Person = String
type Age = Integer

-- A dictionary of people and their age
people :: Dictionary Person Age
people = [("Darth", 46), ("Chewie", 200), ("Yoda", 902)]

-- Given dictionary pas, what is the age of person p?
-- (Returns Nothing if p is not found in the dictionary.)
ageOf :: Dictionary Person Age -> Person -> Maybe Age
ageOf []           _             = Nothing
ageOf ((p',a):pas) p | p == p'   = Just a
                     | otherwise = ageOf pas p

main :: IO ()
main = do
  print $ ageOf people "Chewie"
  print $ ageOf people "Grogu"
