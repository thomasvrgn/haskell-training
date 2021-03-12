import Tree
import Pythagore
import Quaternion

main :: IO()
main = do
  let tree = Node [Leaf (Quaternion 7 5 6 4), Node [Leaf (Quaternion 0 1 2 3), Leaf (Quaternion 4 5 6 7)]]
  print tree
  print $ (+5) <$> tree