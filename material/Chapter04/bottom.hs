import Debug.Trace (trace)

-- Demonstrate the effect of strict pattern matching

data T1 = T1 Int

-- Due to pattern matching, f is strict in its argument
f :: T1 -> Int
f (T1 x) = 42

-- l uses non-strict pattern matching (~‹pat›)
l :: T1 -> Int
l ~(T1 x) = 42

a :: T1
a = trace "a has been evaluated" (T1 0)

-- g is non-strict
g :: Int -> Int
g x = 42

b :: Int
b = trace "b has been evaluated" 0


main :: IO ()
main = do
  -- print $ f undefined        -- result: undefined
  print $ f a                -- result: 42 + side effect
  print $ f (T1 undefined)   -- result: 42
  print $ g undefined        -- result: 42
  print $ g b                -- result: 42
  print $ l a                -- result: 42
  print $ l undefined        -- result: 42
