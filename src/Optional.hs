module Optional where
  data Optional a = Ok a | Undefined deriving (Ord, Eq)

  instance (Show a) => Show (Optional a) where
    show Undefined = "Undefined"
    show (Ok a) = show a

  instance Functor Optional where
    fmap f Undefined = Undefined
    fmap f (Ok a) = (Ok (f a))

  instance Applicative Optional where
    pure = Ok
    Undefined <*> _ = Undefined
    (Ok f) <*> x = f <$> x

  getValue :: Optional a -> a
  getValue (Ok a) = a