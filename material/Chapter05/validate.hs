import Validation

-- type of errors (here: error messages)
type Error = String

-- use p to check whether input inp is OK,
-- otherwise generate message err
validate :: e -> (a -> Bool) -> a -> Validation [e] a
validate err p inp | p inp     = valid inp
                   | otherwise = invalid inp [err]

-- check whether input has minimum/maximum required length,
-- otherwise generate message err
minLength, maxLength :: Int -> Error -> String -> Validation [Error] String
minLength min err = validate err (\inp -> length inp >= min)
maxLength max err = validate err (\inp -> length inp <= max)


validatePass, validateUser :: String -> Validation [Error] String
validatePass pass = minLength  6 "password too short" pass

validateUser user = minLength  6 "username too short" user *>    -- *>: retain only the errors of the lhs
                    maxLength 12 "username too long"  user

-- perform account validation (checks username and password),
-- returns (username,password) pair and accumulated validation error messages
validateAccount :: String -> String -> Validation [Error] (String, String)
validateAccount user pass = pure (,) <*> validateUser user <*> validatePass pass  -- NB: e1 <*> e2 <*> e3 ≡ (e1 <*> e2) <*> e3

main :: IO ()
main = do
  print $ validateAccount "grust"   "foobar"
  print $ validateAccount "grust"   "123"
  print $ validateAccount "torsten" "pit5Nac7cElk7Yev"
