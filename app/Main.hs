import Math
data Node = Node {
  program :: String,
  raw :: String,
  children :: [Node],
  parent :: Maybe Node
}

parser :: [Char] -> Int -> Node -> *
parser code index ast = do
  if length code == index 
  then ast 
  else do
    print $ code !! index
    parser code (index + 1) ast

main = do
  let ast = Node {
    program = "Program",
    raw = "",
    children = [],
    parent = undefined
  }
  let code = "<html><p>test</p><h1>test</h1></html>"
  parser code 0 ast
  print $ pythagore (3, 4, 5)