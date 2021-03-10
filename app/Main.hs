import Tree

main :: IO()
main = do
  let tree = Node 4 (Node 5 Leaf)
  print tree
  print $ fmap (+5) tree