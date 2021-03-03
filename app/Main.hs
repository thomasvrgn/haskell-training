import Complex

main :: IO()
main = do
  let complex1 = Complex 1 0
  let complex2 = Complex 7.4 4
  let complex3 = Complex 1 (-47)
  let complex4 = Complex (-4) 7
  let complex5 = Complex 0 (-4)

  print complex1
  print complex2
  print complex3
  print complex4
  print complex5