{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
  import Mutable

  popLast :: [a] -> [a]
  popLast xs = take ((length xs) - 1) xs

  type Stack = [String]

  empty :: Stack
  empty = []

  shift :: Mutable Stack String
  shift = Mutable \case
    [] -> ("", [])
    (x:xs) -> (x, xs)

  pop :: Mutable Stack String
  pop = Mutable \case
    [] -> ("", [])
    xs -> (last xs, popLast xs)

  push :: String -> Mutable Stack ()
  push str = Mutable \s -> ((), s ++ [str])

  unshift :: String -> Mutable Stack ()
  unshift str = Mutable \s -> ((), str:s)

  stackTest :: String -> Mutable Stack ()
  stackTest user = do
    push user
    unshift "Hello"
    push "!"

  main :: IO()
  main = do
    let mutTest = runMutable (stackTest "Thomas") empty
    print $ mutTest