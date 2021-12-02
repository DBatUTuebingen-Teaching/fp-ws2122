module SetLanguageShallow2Card (IntegerSet,
                                empty,
                                insert,
                                delete,
                                member,
                                card) where

data IntegerSet = IS (Integer -> Bool) Int -- characteristic function

-- constructors
empty :: IntegerSet
empty = IS (\_ -> False) 0

-- empty = IS (const False)

insert :: IntegerSet -> Integer -> IntegerSet
insert (IS f c) x = IS (\y -> x == y || f y)
                       (if f x then c else c + 1)


delete :: IntegerSet -> Integer -> IntegerSet
delete (IS f c) x = IS (\y -> y /= x && f y)
                       (if f x then c - 1 else c)

-- observers
member :: IntegerSet -> Integer -> Bool
member (IS f _) x = f x
-- member (IS f _) = f

card :: IntegerSet -> Int
card (IS _ c) = c
