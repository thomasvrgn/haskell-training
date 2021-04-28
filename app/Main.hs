{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
  prod :: Applicative f => f a -> f b -> f (a, b)
  prod a b = (,) <$> a <*> b

  app :: Applicative f => f (a -> b) -> f a -> f b
  app f x = fmap (\(f', x') -> f' x') (prod f x)

  test = \s -> Just s

  main :: IO ()
  main = print $ app (Just (+5)) (Just 2)