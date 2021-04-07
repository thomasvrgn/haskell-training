{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
  import Mutable

  popLast :: [a] -> [a]
  popLast xs = take ((length xs) - 1) xs

  type Stack = [String]

  empty :: Stack
  empty = []

  unshift :: Mutable Stack String
  unshift = Mutable \case
    [] -> ("", [])
    (x:xs) -> (x, xs)

  pop :: Mutable Stack String
  pop = Mutable \case
    [] -> ("", [])
    xs -> (last xs, popLast xs)

  push :: String -> Mutable Stack ()
  push str = Mutable \s -> ((), s ++ [str])

  stackTest :: String -> Mutable Stack ()
  stackTest user = do
    push "Hello"
    push user

  main :: IO()
  main = do
    let mutTest = runMutable (stackTest "Thomas") empty
    print $ mutTest