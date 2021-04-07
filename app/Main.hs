{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
  import Data.List
  import Mutable
  import Optional

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
    unshift "Bonjour"
    push "!"

  makeSentence :: [String] -> String
  makeSentence xs = concat . intersperse " " $ xs

  factorial :: (Num a, Enum a, Eq a) => a -> a
  factorial 0 = 1
  factorial n = product [1..n]

  neper :: (Num a, Enum a, Eq a, Fractional a) => Optional a -> a
  neper (Ok n) = foldl (\x acc -> x + (1 / factorial acc)) 1 [1..n]
  neper Undefined = neper (Ok 20)

  main :: IO()
  main = do
    let mutTest = getMutable (stackTest "Thomas") empty
    putStrLn $ makeSentence mutTest
    print $ neper Undefined