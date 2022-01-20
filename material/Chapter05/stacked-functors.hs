-- (Excerpt of) characters in "The Force Awakens"
tfa :: [(String, Maybe String)]
tfa = [("Rey" , Nothing),
       ("Han" , Just "Solo"),
       ("Finn", Nothing),
       ("Kylo", Just "Ren")   -- ≡ ("Ben", Just "Solo")
      ]

-- Turn argument into 0 (∀x: zap x = 0)
zap :: a -> Int
zap = const 0

-- Note: you can also write these as
--
--       (fmap . ⋯ . fmap) zap tfa
--       \_______________/
--               |
-- (0 ... depth of functor stack) ×

main :: IO ()
main = do
  print $ zap tfa                              -- ➊
  print $ fmap zap tfa                         -- ➋
  print $ fmap (fmap zap) tfa                  -- ➌
  print $ fmap (fmap (fmap zap)) tfa           -- ➍
  print $ fmap (fmap (fmap (fmap zap))) tfa    -- ➎

-----------------------------------------------------------------------
-- Types:

-- ➋ fmap zap                      :: [(String, Maybe [Char])] -> [Int]
-- ➌ fmap (fmap zap)               :: [(String, Maybe [Char])] -> [(String, Int)]
-- ➍ fmap (fmap (fmap zap))        :: [(String, Maybe [Char])] -> [(String, Maybe Int)]
-- ➎ fmap (fmap (fmap (fmap zap))) :: [(String, Maybe [Char])] -> [(String, Maybe [Int])]

