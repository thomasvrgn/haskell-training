module Groups where
  newtype Addition a = Addition { getAddition :: a }
    deriving (Show)
  newtype Product a = Product { getProduct :: a }
    deriving (Show)

  instance Num a => Semigroup (Addition a) where
    (Addition x) <> (Addition y) = Addition (x + y)

  instance Num a => Semigroup (Product a) where
    (Product x) <> (Product y) = Product (x * y)

  instance Num a => Monoid (Addition a) where
    mempty = Addition 0
    mappend = (<>)

  instance Num a => Monoid (Product a) where
    mempty = Product 1
    mappend = (<>)