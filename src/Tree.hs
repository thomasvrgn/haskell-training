module Tree where
  data Tree a = Node [(Tree a)] | Item a deriving (Show)

  instance Functor Tree where
    fmap f (Item a) = Item (f a)
    fmap f (Node list) = Node (map (fmap f) list)
