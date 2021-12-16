-- Demonstrates sharing during normal order graph reduction
--
-- Uses trace :: String -> a -> a

import Debug.Trace (trace)

sqr :: Num a => a -> a
sqr x = x * x

a :: Int
a = trace "a has been evaluated" (1 + 3)

main :: IO ()
main = print (sqr a)
