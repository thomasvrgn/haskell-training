import Tree
import Quaternion
import Addition

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
  let text = 54 :: (Num a) => a
  print $ (+) <*> pure 5 <$> Addition 41
  print $ Addition 41 >>= (+5)
