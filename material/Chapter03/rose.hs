import qualified M as Rose (inner, leaf, children)
import M (Rose)

-- construct rose tree t:   a
--                         / \
--                        b   c
t :: Rose.Rose Char
t = Rose.inner 'a' [Rose.leaf 'b', Rose.leaf 'c']

-- possible only if M exports constructor Node
-- t = Node 'a' [Node 'b' [], Node 'c' []]


-- number of nodes in t
nodes :: Rose a -> Integer
nodes t = 1 + sum (map nodes (Rose.children t))

-- possible only if M exports constructor Node
-- nodes (Node _ xs) = 1 + sum (map nodes xs)

main :: IO ()
main = do
  print t         -- Show instance for Rose implicitly exported/imported
  print (nodes t)

