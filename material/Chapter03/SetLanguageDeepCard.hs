module SetLanguageDeepCard (IntegerSet(Empty, Insert, Delete),
                            member,
                            card) where

-- constructors

data IntegerSet =
    Empty
  | Insert IntegerSet Integer
  | Delete IntegerSet Integer
  deriving (Show)

-- TODO: build a suitable Show instance for IntegerSet


-- observers

member :: IntegerSet -> Integer -> Bool
member Empty         _ = False
member (Insert xs x) y = x == y || member xs y
member (Delete xs x) y = x /= y && member xs y

card :: IntegerSet -> Int
card Empty         = 0
card (Insert xs x) | member xs x = card xs
                   | otherwise   = card xs + 1
card (Delete xs x) | member xs x = card xs - 1
                   | otherwise   = card xs

