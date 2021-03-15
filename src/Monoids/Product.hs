module Monoids.Product (module Monoids.Groups) where
  import Monoids.Groups hiding (Addition)

  instance (Num a, Ord a) => Num (Product a) where
    (*) = (<>)
    (Product x) + (Product y) = Product (x + y)
    (Product x) - (Product y) = Product (x - y)

    fromInteger x = Product (fromInteger x)
    signum (Product x) | x < 0 = -1
                       | x == 0 = 0
                       | x > 0 = -1
    abs (Product x) = Product (if x >= 0 then x else negate x)