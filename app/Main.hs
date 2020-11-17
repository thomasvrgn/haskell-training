import Math
data User = User {
  username :: String,
  email :: String,
  age :: Int
}

main = do
  let thomas = User {
    username = "Thomas",
    email = "thomas@quark-lang.dev",
    age = 15
  }
  print $ username thomas