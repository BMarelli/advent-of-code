module Common where

type Input = ([Int], [Int])

each :: (a -> b) -> (a, a) -> (b, b)
each f (a, b) = (f a, f b)
