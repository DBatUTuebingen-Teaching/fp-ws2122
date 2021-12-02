-- import SetLanguageDeep
import SetLanguageDeepCard

set12 :: IntegerSet
set12 = (((Empty `Insert` 3) `Insert` 2) `Delete` 3) `Insert` 1

main :: IO ()
main = do
  print $ set12 `member` 3
  print $ card set12

