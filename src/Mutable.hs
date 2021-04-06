module Mutable where
  newtype Mutable s a = Mutable { runState :: s -> (a, s) }