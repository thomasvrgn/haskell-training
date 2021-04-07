{-# LANGUAGE BlockArguments #-}
module Mutable where
  newtype Mutable s a = Mutable { runState :: s -> (a, s) }

  -- We don't want to change state output but only the result
  instance Functor (Mutable s) where
    fmap f (Mutable mut) = Mutable \s ->
      let (a, s') = mut s
        in (f a, s')

  instance Applicative (Mutable s) where
    pure x = Mutable \s -> (x, s)

    (Mutable mutf) <*> (Mutable mut) = Mutable \s ->
      let (f, s') = mutf s
          (a, b) = mut s'
        in (f a, b)
