module SetLanguageShallow2 (IntegerSet,
                            empty,
                            insert,
                            delete,
                            member) where

data IntegerSet = IS (Integer -> Bool) -- characteristic function

-- constructors
empty :: IntegerSet
empty = IS (\_ -> False)
-- empty = IS (const False)

insert :: IntegerSet -> Integer -> IntegerSet
insert (IS f) x = IS (\y -> x == y || f y)

delete :: IntegerSet -> Integer -> IntegerSet
delete (IS f) x = IS (\y -> y /= x && f y)

-- observers
member :: IntegerSet -> Integer -> Bool
member (IS f) x = f x
-- member (IS f) = f
