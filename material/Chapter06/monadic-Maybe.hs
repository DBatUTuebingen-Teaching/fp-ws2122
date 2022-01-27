import Data.Maybe (maybeToList)
import Data.Char (ord)


--instance Monad Maybe where
--  return x = Just x

--  Nothing >>= g = Nothing
--  Just x  >>= g = g x


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
chinese' :: String -> Maybe Char
chinese' n = numeralToDigit n >>= digitToVal >>= chineseNumeral


main :: IO ()
main = putStrLn $ maybeToList $ chinese' "five"
