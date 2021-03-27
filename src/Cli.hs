module CLI where
  data User = User { name :: String, age :: Integer }
    deriving (Show, Eq)

  input :: String -> IO String
  input str = do
    putStr str
    hFlush stdout
    getLine

  split :: Char -> String -> [String]
  split d [] = []
  split d s = x : split d (drop 1 y)
    where (x,y) = span (/= d) s

  cli :: Optional User -> IO()
  cli Undefined = do
    putStrLn "Création de l'utilisateur..."
    username :: String <- input "Nom d'utilisateur: "
    age :: Integer <- read <$> input "Age: "
    cli (Ok User { name = username, age = age })

  cli (Ok user) = do
    res <- input ">>> "
    let command = takeWhile (/= ' ') res
    let args = drop 1 $ dropWhile (/= ' ') res
    let (username, age) = let [username, age] = split ' ' args in (username, read age :: Integer)

    case command of
      "welcome" -> welcome user >> cli (Ok user)
      "modify" -> cli (Ok (update username age))
      _ -> putStrLn "Cette commande n'existe pas !" >> cli (Ok user)

  update :: String -> Integer -> User
  update name age = User { name = name, age = age }

  welcome :: User -> IO()
  welcome user = putStrLn $ "Bonjour " ++ name user ++ ", vous avez " ++ show (age user) ++ " ans."


