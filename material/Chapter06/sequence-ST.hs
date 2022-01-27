import Data.Maybe (fromMaybe)
import Data.Char (ord)

-- State transformer
-- (a function of type a -> ST b yields a result of type b and
--  and a following state)
type ST b  = State -> (State, b)
type State = String


numeralToDigit :: String -> ST Char
numeralToDigit w = \s -> (s ++ "numeralToDigit ", fromMaybe '0' (lookup w digits))
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

digitToVal :: Char -> ST Int
digitToVal d | d `elem` ['0'..'9'] = \s -> (s ++ "digitToVal ", ord d - ord '0')

chineseNumeral :: Int -> ST Char
chineseNumeral n = \s -> (s ++ "chineseNumeral ", "零一二三四五六七八九" !! n)


-- Left-to-right composition for stateful functions
(>=>) :: (a -> ST b) -> (b -> ST c) -> (a -> ST c)
f >=> g = \x s0 -> let (s1, y) = f x s0 in
                   let (s2, z) = g y s1 in
                   (s2, z)

-- Reformulation of the English numeral to Chinese digit conversion
chinese' :: String -> ST Char
chinese' = numeralToDigit >=> digitToVal >=> chineseNumeral


-- Convenience: Run a stateful computation:
-- apply f to initial state (empty log ""), extract f's result (and ignore final state)
runST :: (a -> ST b) -> a -> b
runST f x = snd $ f x ""


main :: IO ()
main = do
  print    $ chinese' "five" ""
  putStrLn $ [runST chinese' "five"]
