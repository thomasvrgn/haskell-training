{-#LANGUAGE ScopedTypeVariables#-}
import Tree
import Complex.Quaternion
import Monoids.Addition
import Monoids.Product
import Control.Monad (unless)
import Semigroups.AntiCommutative
import Control.Monad.State.Lazy
import System.Environment
import System.IO

data User = User { name :: String, age :: Integer }
  deriving (Show, Eq)

data Optional a = Undefined | Optional { getValue :: a }

input :: String -> IO String
input str = do
  putStr str
  hFlush stdout
  getLine

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y)
  where (x,y) = span (/= d) s

cli :: Optional User -> IO()
cli Undefined = do
  putStrLn "Création de l'utilisateur..."
  username :: String <- input "Nom d'utilisateur: "
  age :: Integer <- read <$> input "Age: "
  cli (Optional User { name = username, age = age })

cli (Optional user) = do
  res <- input ">>> "
  let command = takeWhile (/= ' ') res
  let args = drop 1 $ dropWhile (/= ' ') res
  let [username, age] = split ' ' args

  case command of
    "welcome" -> do
      welcome user
      cli (Optional user)
    "modify" -> cli (Optional (update username (read age :: Integer)))

update :: String -> Integer -> User
update name age = User { name = name, age = age }

welcome :: User -> IO()
welcome user = putStrLn $ "Bonjour " ++ name user ++ ", vous avez " ++ show (age user) ++ " ans."

main :: IO()
main = cli Undefined

