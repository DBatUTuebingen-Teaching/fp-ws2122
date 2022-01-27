import Data.Maybe (maybe)
import Data.Char (ord)

-- State transformer
-- (a function of type a -> Logged b yields a log entry and a result of type b)

type Logged b = (Log, b) -- equivalently: type Logged = (,) Log
type Log = String


numeralToDigit :: String -> Logged Char
numeralToDigit w = ("numeralToDigit", maybe '0' id (lookup w digits))
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
digitToVal d | d `elem` ['0'..'9'] = ("digitToVal", ord d - ord '0')

chineseNumeral :: Int -> Logged Char
chineseNumeral n = ("chineseNumeral", "零一二三四五六七八九" !! n)


-- Left-to-right composition for stateful functions
(>=>) :: (a -> Logged b) -> (b -> Logged c) -> (a -> Logged c)
f >=> g = \x -> let (l1, y) = f x in
                let (l2, z) = g y in
                (l1 ++ " " ++ l2, z)


-- Reformulation of the English numeral to Chinese digit conversion
chinese' :: String -> Logged Char
chinese' = numeralToDigit >=> digitToVal >=> chineseNumeral


-- Convenience: Run a logged computation
runLogged :: (a -> Logged b) -> a -> b
runLogged f = snd . f


main :: IO ()
main = do
  print    $ chinese' "five"
  putStrLn $ [runLogged chinese' "five"]
