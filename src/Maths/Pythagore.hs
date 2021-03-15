module Maths.Pythagore where

  pythagore :: (Num a, Eq a) => (a, a, a) -> Bool
  pythagore (x,y,z) = x^2 + y^2 == z^2

  data Triangle a = Triangle a a a deriving (Show, Eq, Ord)

  class TriangleProperties a where
    isRectangle :: a -> Bool
    isBuildable :: a -> Bool

  instance (Num a, Eq a, Floating a, Ord a) => TriangleProperties (Triangle a) where
    isRectangle (Triangle x y z) = x**2 + y**2 == z**2
    isBuildable (Triangle x y z) = highestSide <= ((otherSide !! 0) + (otherSide !! 1))
      where orderedSide = if x > y && x > z then [x, y, z] else if y > x && y > z then [y, x, z] else [z, y, x]
            highestSide = head orderedSide
            otherSide = tail orderedSide
