module Common where

type Input = String
data Multiplication = M Int Int deriving (Show, Eq)
data Instruction a = Do | Dont | V a deriving (Show, Eq)

apply :: Multiplication -> Int
apply (M x y) = x * y
