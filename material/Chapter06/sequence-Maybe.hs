import Data.Maybe (maybeToList)
import Data.Char (ord)

-- A safe variant of (!!)
at :: Int -> [a] -> Maybe a
at i xs | i `elem` [0..length xs-1] = Just (xs !! i)
        | otherwise                 = Nothing


numeralToDigit :: String -> Maybe Char
numeralToDigit w = lookup w digits
  where
    digits = [("null",  '0'),
              ("zero",  '0'),
              ("one",   '1'),
              ("two",   '2'),
              ("three", '3'),
              ("four",  '4'),
              ("five",  '5'),
              ("six",   '6'),
              ("seven", '7'),
              ("eight", '8'),
              ("nine",  '9')]

digitToVal :: Char -> Maybe Int
digitToVal d | d `elem` ['0'..'9'] = Just (ord d - ord '0')
             | otherwise           = Nothing

chineseNumeral :: Int -> Maybe Char
chineseNumeral n = at n "零一二三四五六七八九"


-- Translate English numeral n into a Chinese digit,
-- *if possible*
chinese :: String -> Maybe Char
chinese n = case numeralToDigit n of
              Nothing -> Nothing
              Just d  -> case digitToVal d of
                           Nothing -> Nothing
                           Just v  -> chineseNumeral v


-- Left-to-right composition for partial functions
(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >=> g = \x -> case f x of
                  Nothing -> Nothing
                  Just y  -> g y

-- Reformulation of the English numeral to Chinese digit conversion
chinese' :: String -> Maybe Char
chinese' = numeralToDigit >=> digitToVal >=> chineseNumeral


main :: IO ()
main = do
  putStrLn $ show $ chinese' "five"
  putStrLn $ show $ chinese' "fove"
