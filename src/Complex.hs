module Complex where

infix 6 :+

data Complex a
  = a :+ a
  deriving (Eq)

showImaginaryPart :: (Num a, Ord a, Show a) => a -> String
showImaginaryPart x | x == 1 = "i"
                    | x == -1 = "i"
                    | x == 0 = ""
                    | x < 0 = show (abs x) ++ "i"
                    | x > 0 = show x ++ "i"

showSignum :: (Num a, Ord a, Show a) => a -> a -> String
showSignum x y | x == 0 && y < 0 = "-"
               | x > 0 && y > 0 = " + "
               | x < 0 && y < 0 = " - "
               | otherwise = ""

showRealPart :: (Num a, Ord a, Show a) => a -> String
showRealPart x | x /= 0 = show x
               | x == 0 = ""

instance (Show a, Ord a, Num a) => Show (Complex a) where
  show (a:+b) = showRealPart a ++ (showSignum a b) ++ showImaginaryPart b

instance (RealFloat a) => Num (Complex a) where
  (a:+b) + (a':+b') = (a+a') :+ (b+b')