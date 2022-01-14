{-# LANGUAGE TupleSections #-}

module MergingMaps where

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Data types (provided)

type Feature = Char

type CharMap = [String]

-- | Coordinates in a map map
--   (row, column) distance from the upper left corner,
--   each starting from 0.
type Coord = (Int,Int)

-- | An 'Offset r c' describes a map shift of
--   r rows and c columns (negative shifts are possible)
data Offset = Offset Int Int
  deriving (Eq)

instance Show Offset where
    show (Offset r c) = show (r, c)

instance Ord Offset where
    (Offset oRow1 oCol1) <= (Offset oRow2 oCol2) = oRow1 <= oRow2 && oCol1 <= oCol2

-- | A map map.
data Map = Map Int                   -- ^ height of the map in number of rows
                 Int                   -- ^ width of the map in number of columns
                 (M.Map Coord Feature) -- ^ coordinates of major features
  deriving (Show, Eq)

type Score = Int

--------------------------------------------------------------------------------
-- Example input (provided)

m1 :: CharMap
m1 = [ "--A-C"
     , "----D"
     , "----B"
     ]

m2 :: CharMap
m2 = [ "C----"
     , "D---F"
     , "B----"
     ]

m3 :: CharMap
m3 = [ "C----"
     , "-----"
     , "B-A-C"
     ]

m4 :: CharMap
m4 = [ "----D"
     , "-E--B"
     , "-----"
     ]

m5 :: CharMap
m5 = [ "-D--C"
     , "----G"
     , "----B"
     ]

maps :: [CharMap]
maps = [ m1, m2, m3, m4, m5 ]

-------------------------------------------------------------------------------
-- Converting raw maps from and to internal maps (provided)

-- | Convert a raw map into our internal tree-based map.
fromCharMap :: CharMap -> Map
fromCharMap []      = error "empty map"
fromCharMap t@(r:_) = Map rows cols (M.fromList features)
  where
    rows     = length t
    cols     = length r
    features = concat [ [ ((i, j), f) | isMajorFeature f ]
                      | (row, i) <- zip t [0..]
                      , (f, j)   <- zip row [0..]
                      ]

isMajorFeature :: Feature -> Bool
isMajorFeature c = c `elem` ['A'..'Z']

-- | Convert the internal map representation into a character map.
toCharMap :: Map -> CharMap
toCharMap (Map tRows tCols tFeatures)
    = [ [ fromMaybe '-' (M.lookup (row, col) tFeatures)
        | col <- [0..tCols - 1]
        ]
      | row <- [0..tRows - 1]
      ]

-- | Render the character map into a single string.
renderCharMap :: CharMap -> String
renderCharMap = intercalate "\n"

-- | Render multiple maps into a single string.
showMaps :: [Map] -> String
showMaps = intercalate "\n++\n" . map (renderCharMap . toCharMap)

-------------------------------------------------------------------------------
-- Code to be written


