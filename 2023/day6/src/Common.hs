module Common where

newtype Miliseconds = Ms {getTime :: Int} deriving (Show, Eq)
newtype Milimeters = Mm {getDistance :: Int} deriving (Show, Eq)

data Race = R {time :: Miliseconds, distance :: Milimeters} deriving (Show, Eq)
type Input = [Race]
