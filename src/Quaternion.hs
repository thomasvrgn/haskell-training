{-# LANGUAGE GADTs #-}
module Quaternion where
  data Quaternion a where
    Quaternion :: (Num a) => a -> a -> a -> a -> Quaternion a

  instance (RealFloat a, Num a, Fractional a) => Fractional (Quaternion a) where
    (Quaternion a b c d) / z@(Quaternion a' b' c' d') =
      Quaternion ((a * a' + b * b' + c * c' + d * d') / zNorm)
                 ((-a * b' + b * a' - c * d' + d * c') / zNorm)
                 ((-a * c' + b * d' + c * a' - d * b') / zNorm)
                 ((-a * d' - b * c' + c * b' + d * a') / zNorm)
      where
        zNorm = norm z

    fromRational a = Quaternion (fromRational a) 0 0 0

  instance (RealFloat a) => Num (Quaternion a) where
    (Quaternion a b c d) + (Quaternion a' b' c' d') =
      Quaternion (a + a') (b + b') (c + c') (d + d')

    (Quaternion a b c d) - (Quaternion a' b' c' d') =
      Quaternion (a - a') (b - b') (c - c') (d - d')

    (Quaternion a b c d) * (Quaternion a' b' c' d') =
      Quaternion (a * a' - b * b' - c * c' - d * d')
                 (a * b' + b * a' + c * d' - d * c')
                 (a * c' - b * d' + c * a' + d * b')
                 (a * d' + b * c' - c * b' + d * a')

    abs z = Quaternion (sqrt (norm z)) 0 0 0
    fromInteger a = Quaternion (fromInteger a) 0 0 0
    signum z = z / abs z

  norm :: (Num a, Floating a) => Quaternion a -> a
  norm (Quaternion a b c d) = (a**2) + (b**2) + (c**2) + (d**2)

  non :: (Num a) => Quaternion a -> Quaternion a
  non (Quaternion a b c d) = Quaternion a (-b) (-c) (-d)

  realPart :: (Num a) => Quaternion a -> a
  realPart (Quaternion a _ _ _) = a
