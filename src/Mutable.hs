module Mutable where
  newtype Mutable s a = { runState :: s -> (a, s) }