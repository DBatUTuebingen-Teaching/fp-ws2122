import Data.List
import Data.Char

let text = cycle ["stop","reading","!","Because","this","is","an","endless","text","that","-","once","in","a","loop","-","will","never","allow","you","to"]
let search = map (map toLower) ["A", "text", "loop", "!"]

length search
-- :sprint text
-- :sprint search

let finding =  search `intersect` text
-- :sprint finding
-- :sprint text
-- :sprint search

length finding
-- :sprint finding
-- :sprint text
-- :sprint search
