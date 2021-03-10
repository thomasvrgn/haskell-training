import Tree

main :: IO()
main = do
  let tree = Node [(Item 5), (Node [(Item 1)])]
  print tree
  print $ pure (+) <*> (Node [Item 4, Item 5]) <*> tree