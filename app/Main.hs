import Complex
import Optional
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Directory

argumentParser :: Optional String -> String
argumentParser Undefined = "tetst"
argumentParser n = getValue n

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

join_path :: [[Char]] -> [Char]
join_path paths = slice 1 (length str) str
  where str = (foldl (\x y -> x ++ "/" ++ y) "" paths)

main :: IO()
main = do
  arguments <- getArgs
  if length arguments == 0 then
    putStrLn $ argumentParser Undefined
  else
    putStrLn $ argumentParser (Ok (arguments !! 0))

  print $ join_path ["src", "core", "index.js"]
