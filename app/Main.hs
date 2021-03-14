import Tree
import Quaternion
import Groups

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

evalPolish :: (Num a, Read a, Fractional a) => String -> a
evalPolish list = (head . foldl processItem [] . words) list
  where processItem (x:y:ys) "+" = (x + y):ys
        processItem (x:y:ys) "-" = (y - x):ys
        processItem (x:y:ys) "*" = (x * y):ys
        processItem (x:y:ys) "/" = (x / y):ys
        processItem xs number = read number:xs

main = do
  print "test"
  print $ Addition 5 + Addition 5
  print $ Addition 5 * Addition 4
  print $ Addition 2 - Addition 5
  print $ abs (Addition (-5))
  print "test"
  print $ (+) <*> pure 5 <$> Addition 41
  print $ Addition 41 >>= (+5)
