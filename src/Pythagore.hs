module Pythagore where

pythagore :: (Num a, Eq a) => (a, a, a) -> Bool
pythagore (x,y,z) = x^2 + y^2 == z^2