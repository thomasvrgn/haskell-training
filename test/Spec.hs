import Quaternion

main :: IO()
main = do
  print $ (Quaternion 0 1 2 3) == (Quaternion 0 1 2 3)
