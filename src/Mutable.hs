{-# LANGUAGE BlockArguments #-}
module Mutable where
  newtype Mutable s a = Mutable { runState :: s -> (a, s) }

  -- We don't want to change state output but only the result
  instance Functor (Mutable s) where
    fmap f (Mutable mut) = Mutable \s ->
      let (a, s') = mut s
        in (f a, s')
