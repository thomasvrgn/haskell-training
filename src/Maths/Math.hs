module Maths.Math (
  module Maths.Pythagore
) where

import Maths.Pythagore

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