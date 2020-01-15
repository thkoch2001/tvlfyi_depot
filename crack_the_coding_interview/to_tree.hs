data Tree a = Node a [Tree a] deriving (Show)

withRoot :: [a] -> [Tree a]
withRoot xs = xs |> toThing |> fmap buildTree

buildTree :: (a, [a])


toTree :: [a] -> Tree a
toTree [x]      = Node x []
toTree [x | xs] = Node x (toTree xs)
