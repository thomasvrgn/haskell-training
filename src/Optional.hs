module Optional where
  data Optional a = Ok a | Undefined deriving (Ord, Eq)

  instance (Show a) => Show (Optional a) where
    show Undefined = "Undefined"
    show (Ok a) = show a

  getValue :: Optional a -> a
  getValue (Ok a) = a