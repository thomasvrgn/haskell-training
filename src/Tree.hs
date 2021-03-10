module Tree where
  data Tree a = Leaf | Node a (Tree a) deriving (Show)

  instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node a list) = Node (f a) (fmap f list)