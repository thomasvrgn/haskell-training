{-# LANGUAGE GADTs #-}
module Quaternion where
data Quaternion a where
  Quaternion :: (Num a) => a -> a -> a -> a -> Quaternion a

instance (RealFloat a) => Num (Quaternion a) where
  (Quaternion a b c d) + (Quaternion a' b' c' d') =
    Quaternion (a + a') (b + b') (c + c') (d + d')