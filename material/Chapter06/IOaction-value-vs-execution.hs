-- In function g, the IO actions getInt are regular Haskell values.
-- Referential transparency applies, the two getInt values bound to
-- x and y are identical.

getInt :: IO Integer
getInt = fmap read getLine

f :: IO [Integer]
f = do x <- getInt   -- ⎫   this can NOT       do x <- getInt
       y <- getInt   -- ⎬   be rewritten
       return [x,y]  -- ⎭   into                  return [x,x]

g :: [IO Integer]
g = let x = getInt   -- ⎫   this can be        let x = getInt
        y = getInt   -- ⎬   rewritten
     in [x,y]        -- ⎭   into                in [x,x]

main :: IO ()
main = do xs <- f
          print xs

-- main = do xs <- sequence g
--           print xs
