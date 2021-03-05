import Complex
import Optional

import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Directory

showHelp :: IO String
showHelp = do
  putStrLn "Haskell CLI Example"
  putStrLn " - Version: 0.0.1"
  putStrLn " - Description: A simple CLI in Haskell\n"
  return ""

argumentParser :: Optional [String] -> IO String
argumentParser Undefined = showHelp
argumentParser n = do
  putStrLn $ show (getValue n)

  return $ (getValue n) !! 0

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

join_path :: [[Char]] -> [Char]
join_path paths = slice 1 (length str) str
  where str = (foldl (\x y -> x ++ "/" ++ y) "" paths)

isPrime :: Integer -> Bool
isPrime n = length (factors n) <= 2
  where factors n = [ x | x <- [1..n], n `mod` x == 0 ]

main :: IO()
main = do
  arguments <- getArgs
  argumentParser (if length arguments == 0 then Undefined else (Ok arguments))
  print $ join_path ["src", "core", "index.js"]
  print $ isPrime 15
  print $ isPrime 2
  print $ isPrime 1
  print $ isPrime 17
