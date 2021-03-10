module Tree where
  data Tree a = Leaf | Node a (Tree a) deriving (Show)