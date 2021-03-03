import Complex
import Optional

main :: IO()
main = do
  let test = Ok (Complex 6 6)

  print $ getValue test
  print $ realPart (getValue test)

