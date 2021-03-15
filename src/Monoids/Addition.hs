module Monoids.Addition (module Monoids.Groups) where
  import Monoids.Groups hiding (Product)
  instance (Num a, Enum a, Ord a) => Num (Addition a) where
    (+) = (<>)
    x * (Addition y) = foldl (+) 0 [ x | _ <- [1..y] ]
    (Addition x) - (Addition y) = Addition (x - y)

    fromInteger x = Addition (fromInteger x)
    abs (Addition x) = Addition (if x >= 0 then x else negate x)
    signum (Addition x) | x < 0 = -1
                        | x == 0 = 0
                        | x > 0 = 1

  instance Functor Addition where
    fmap f (Addition x) = (Addition (f x))

  instance Applicative Addition where
    pure = Addition
    (Addition f) <*> x = fmap f x

  instance Monad Addition where
    return = Addition
    Addition x >>= f = f x