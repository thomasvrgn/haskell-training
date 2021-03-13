import Tree
import Quaternion

newtype Value a = Value { getValue :: a }
  deriving (Show)

instance Num a => Semigroup (Value a) where
  Value x <> Value y = Value (x + y)

instance Num a => Monoid (Value a) where
  mempty = Value 0
  Value x `mappend` Value y = Value (x + y)


evalPolish :: (Num a, Read a, Fractional a) => String -> a
evalPolish list = (head . foldl processItem [] . words) list
  where processItem (x:y:ys) "+" = (x + y):ys
        processItem (x:y:ys) "-" = (y - x):ys
        processItem (x:y:ys) "*" = (x * y):ys
        processItem (x:y:ys) "/" = (x / y):ys
        processItem xs number = read number:xs

main = do
  print $ evalPolish "5 3 -"
  print $ "test"

  let val = Value 5
  let val1 = Value 10
  print $ getValue val
  print $ val `mappend` val1


