module SetLanguageShallow1 (IntegerSet,
                            empty,
                            insert,
                            delete,
                            member) where

data IntegerSet = IS [Integer] -- unordered, duplicates allowed

-- constructors
empty :: IntegerSet
empty = IS []

insert :: IntegerSet -> Integer -> IntegerSet
insert (IS xs) x = IS (x:xs)

delete :: IntegerSet -> Integer -> IntegerSet
delete (IS xs) x = IS (filter (/= x) xs)

-- observer
member :: IntegerSet -> Integer -> Bool
member (IS xs) x = elem x xs
