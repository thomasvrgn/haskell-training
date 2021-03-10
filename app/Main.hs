import Tree

main :: IO()
main = do
  let tree = Node [Leaf 5, Node [Leaf 1]]
  print tree
  print $ pure (+) <*> Node [Leaf 4, Leaf 5] <*> tree