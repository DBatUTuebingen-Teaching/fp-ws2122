import Prelude hiding (fst)

fst :: (a,b) -> a
fst (x,y) = x

sqr :: Num a => a -> a
sqr x = x * x

-- ⚠️ Applictative order reduction of this expression will not terminate
bomb :: String
bomb = '💣' : bomb

main :: IO ()
main = do
  print $ fst (sqr (1 + 3), bomb)
  -- activiate at your risk ... (interrupt with Control-C)
  -- putStrLn bomb
