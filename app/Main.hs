import Complex

main :: IO()
main = do
  let complex1 = Complex 1 6
  let complex2 = Complex 7.4 4

  putStrLn $ "Complex operations on " ++ show complex1 ++ " and " ++ show complex2

  putStrLn $ "Mulitplication: " ++ show (complex1 + complex2)
  putStrLn $ "Subtraction: " ++ show (complex1 - complex2)
  putStrLn $ "Addition: " ++ show (complex1 + complex2)
  putStrLn $ "Division: " ++ show (complex1 / complex2)
