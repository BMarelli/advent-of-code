module Common where

newtype Miliseconds = Ms Int deriving (Show, Eq)
newtype Milimeters = Mm Int deriving (Show, Eq)

data Race = R {time :: Miliseconds, distance :: Milimeters} deriving (Show, Eq)
type Input = [Race]
