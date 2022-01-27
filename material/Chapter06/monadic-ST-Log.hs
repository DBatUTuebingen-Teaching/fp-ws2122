import Data.Maybe (maybe)
import Data.Char (ord)

-- State transformer
-- (a function of type a -> Logged b yields a result of type b and
--  and a log entry)
data Logged b =
  Logged b Log
  deriving (Show)

type Log = String

instance Monad Logged where
  return x = Logged x ""

  (Logged x l1) >>= g = case g x of
                          Logged y l2 -> Logged y (l1 ++ " " ++ l2)


numeralToDigit :: String -> Logged Char
numeralToDigit w = Logged (maybe '0' id (lookup w digits)) "numeralToDigit"
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

digitToVal :: Char -> Logged Int
digitToVal d | d `elem` ['0'..'9'] = Logged (ord d - ord '0') "digitToVal"

chineseNumeral :: Int -> Logged Char
chineseNumeral n = Logged ("零一二三四五六七八九" !! n) "chineseNumeral"



-- Reformulation of the English numeral to Chinese digit conversion
chinese' :: String -> Logged Char
chinese' n = numeralToDigit n >>= digitToVal >>= chineseNumeral


-- Convenience: Run a logged computation
runLogged :: (a -> Logged b) -> a -> b
runLogged f x = case f x of
                  Logged y _ -> y


main :: IO ()
main = do
  print $ chinese' "five"
  putChar $ runLogged chinese' "five"
