module Common where

type Input = String
data Multiplication = M Int Int deriving (Show)

apply :: Multiplication -> Int
apply (M x y) = x * y
