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

  -- Simply running the State and getting the corresponding output

  getResult :: Mutable s a -> s -> a
  getResult f s = fst $ runMutable f s

  getMutable :: Mutable s a -> s -> s
  getMutable f s = snd $ runMutable f s

  -- Class for mutable with s State type
  class MutableOperations s where
    get :: Mutable s s
    get = Mutable \s -> (s, s)

    put :: s -> Mutable s ()
    put s = Mutable \_ -> ((), s) -- Put returns anything

  -- Default MutableOperations instance for Mutable do not require to reimplement all methods
  instance MutableOperations (Mutable s s)