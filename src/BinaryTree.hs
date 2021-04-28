module BinaryTree where
  data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) (BinaryTree a)
    deriving (Show)
  instance Functor BinaryTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch x y) = Branch (f <$> x) (f <$> y)

  instance Applicative BinaryTree where
    pure = Leaf
    (Leaf f) <*> x = f <$> x
    (Branch x xs)<*> (Branch y ys) = Branch (x <*> y) (xs <*> ys)
    (Branch f f') <*> (Leaf x) = Branch (f <$> x) (f' <*> x)

  instance Monad BinaryTree where
    return = pure
    Leaf x >>= f = (f x)
    Branch x xs >>= f = Branch (x >>= f) (xs >>= f)

