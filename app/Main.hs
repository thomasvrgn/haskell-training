import Tree
import Complex.Quaternion
import Monoids.Addition
import Monoids.Product
import Control.Monad (unless)
import Semigroups.AntiCommutative
import Control.Monad.State.Lazy

qtprocess :: Num a => Quaternion a
qtprocess = do
  qt <- Quaternion 0 1 2 3
  return $ qt *2

main :: IO()
main = do
  print $ qtprocess
