import Data.List (nub)
import Network.HTTP.Types  --  ⚠️ requires library http-types, install via: stack install http-types-0.12.3

-- Flagged has kind * -> *
type Flagged = (,) Bool                   -- Flagged a ≡ (Bool, a)

-- does HTTP status code s indicate an error condition?
okStatus :: Status -> Flagged Status
okStatus s = (s < status400, s)

main :: IO ()
main = print $ nub [ m | (True, Status _ m) <- map okStatus [status200..status501] ]
