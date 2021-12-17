
type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char


solve :: Grid -> [Grid]
solve = filter valid . expand . many prune . choices 

choices :: Grid -> Matrix [Digit]


expand :: Matrix [Digit] -> [Grid]


valid :: Grid -> Bool


prune :: Matrix [Digit] -> Matrix [Digit]


many :: (Eq a) => (a -> a) -> a -> a


-- A simple example Sudoku to solve
sudokuSimple =
  [ "057680002"
  , "600249070"
  , "904000618"
  , "500906830"
  , "036402001"
  , "140300260"
  , "090027100"
  , "068090703"
  , "705063029"
  ]
