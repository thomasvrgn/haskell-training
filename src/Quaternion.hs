{-# LANGUAGE GADTs #-}
module Quaternion where
data Quaternion a where
  Quaternion :: (Num a) => a -> a -> a -> a -> Quaternion a

instance (RealFloat a) => Num (Quaternion a) where
  (Quaternion a b c d) + (Quaternion a' b' c' d') =
    Quaternion (a + a') (b + b') (c + c') (d + d')

  (Quaternion a b c d) * (Quaternion a' b' c' d') =
    Quaternion
      (a * a' - b * b' - c * c' - d * d')
      (a * b' + b * a' + c * d' - d * c')
      (a * c' - b * d' + c * a' + d * b')
      (a * d' + b * c' - c * b' + d * a')

  abs z = Quaternion (sqrt (norm z)) 0 0 0

norm :: (Num a, Floating a) => Quaternion a -> a
norm (Quaternion a b c d) = (a**2) + (b**2) + (c**2) + (d**2)

-- a => a * a' - b * b' - c * c' - d * d',
-- bi => a * b' + b * a' + c * d' - d * c',
-- cj => a * c' - b * d' + c * a' + d * b',
-- dk => a * d' + b * c' - c * b' + d * a'