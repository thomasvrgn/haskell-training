import Tree
import Complex.Quaternion
import Monoids.Addition
import Monoids.Product
import Control.Monad (unless)
import Semigroups.AntiCommutative
import Algebra.Ring hiding ((*))

sqrt' :: (Num a, Eq a, Floating a) => a -> a -> a
sqrt' dec 0 = 1
sqrt' dec x = x1 - ((x1**2 - dec) / (2*x1))
  where x1 = sqrt' dec (x - 1)

evalPolish :: (Num a, Read a, Fractional a) => String -> a
evalPolish list = (head . foldl processItem [] . words) list
  where processItem (x:y:ys) "+" = (x + y):ys
        processItem (x:y:ys) "-" = (y - x):ys
        processItem (x:y:ys) "*" = (x * y):ys
        processItem (x:y:ys) "/" = (x / y):ys
        processItem xs number = read number:xs

hyperPower :: Integer -> Integer
hyperPower x = foldl (\x y -> (x * x)) x [1..x]

data Optional a = Undefined | Ok { getValue :: a } deriving (Ord, Eq)
newtype Sum a = Sum { getSum :: a } deriving (Show)

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance (Show a) => Show (Optional a) where
  show Undefined = "Undefined"
  show (Ok a) = "Ok " ++ (show a)

instance (Semigroup a) => Semigroup (Optional a) where
  Ok a <> Ok b = Ok (a <> b)

main = do
  let opt = Ok (Sum 7)
  print opt
  (print . getSum . getValue) opt
  (print . getSum . getValue) (opt <> (Ok (Sum 7)))