module M (Rose, leaf, inner, label, children) where

data Rose a = Node a [Rose a]
  deriving (Show, Eq)

leaf :: a -> Rose a
leaf x = Node x []

inner :: a -> [Rose a] -> Rose a
inner x xs = Node x xs

label :: Rose a -> a
label (Node x _) = x

children :: Rose a -> [Rose a]
children (Node _ xs) = xs
