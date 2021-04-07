{-# LANGUAGE BlockArguments, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Mutable where
  newtype Mutable s a = Mutable { runMutable :: s -> (a, s) }

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

  instance Monad (Mutable s) where
    return = pure

    (Mutable mut) >>= f = Mutable \s ->
      let (a, s') = mut s
        in runMutable (f a) s'

  class Monad m => MutableOperations s m where
    get :: m s
    put :: s -> m ()

  instance MutableOperations s (Mutable s) where
    get = Mutable \s -> (s, s)
    put s = Mutable \_ -> ((), s)