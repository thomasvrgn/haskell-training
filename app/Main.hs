import Tree
import Complex.Quaternion
import Monoids.Addition
import Monoids.Product
import Control.Monad (unless)

sqrt' :: (Num a, Eq a, Floating a) => a -> a -> a
sqrt' dec 0 = 1
sqrt' dec x = x1 - ((x1**2 - dec) / (2*x1))
  where x1 = sqrt' dec (x - 1)

squareroot :: (Num a, Floating a, Eq a) => a -> a
squareroot x = sqrt' x 10

evalPolish :: (Num a, Read a, Fractional a) => String -> a
evalPolish list = (head . foldl processItem [] . words) list
  where processItem (x:y:ys) "+" = (x + y):ys
        processItem (x:y:ys) "-" = (y - x):ys
        processItem (x:y:ys) "*" = (x * y):ys
        processItem (x:y:ys) "/" = (x / y):ys
        processItem xs number = read number:xs

main = do
  print "test"
  print $ Product 4 * Product 5
  mapM_ (\x -> putStrLn (show x ++ ": " ++ show (squareroot x))) [1..10]
