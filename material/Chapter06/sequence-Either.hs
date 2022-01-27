import Data.Maybe (maybe)
import Data.Either (either)
import Data.Char (ord)

-- Exc a is either an exception (Left err) or a value (Right y)
type Exc a = Either Error a
type Error = String

-- A safe variant of (!!)
at :: Int -> [a] -> Exc a
at i xs | i `elem` [0..length xs-1] = Right (xs !! i)
        | otherwise                 = Left "list index out of bound"


numeralToDigit :: String -> Exc Char
numeralToDigit w = maybe (Left "unknown numeral")
                         Right
                         (lookup w digits)
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

digitToVal :: Char -> Exc Int
digitToVal d | d `elem` ['0'..'9'] = Right (ord d - ord '0')
             | otherwise           = Left "non-digit has no value"

chineseNumeral :: Int -> Exc Char
chineseNumeral n = at n "零一二三四五六七八九"


-- Translate English numeral n into a Chinese digit,
-- *if possible* (return an error message otherwise)
chinese :: String -> Exc Char
chinese n = case numeralToDigit n of
              Left err -> Left err
              Right d  -> case digitToVal d of
                            Left msg -> Left msg
                            Right v  -> chineseNumeral v


-- Left-to-right composition for partial functions
(>=>) :: (a -> Exc b) -> (b -> Exc c) -> (a -> Exc c)
f >=> g = \x -> case f x of
                  Left msg -> Left msg
                  Right y  -> g y

-- Reformulation of the English numeral to Chinese digit conversion
chinese' :: String -> Exc Char
chinese' = numeralToDigit >=> digitToVal >=> chineseNumeral


main :: IO ()
main = do
  putStrLn $ either id (:"") $ chinese  "six"
  putStrLn $ either id (:"") $ chinese' "sux"
