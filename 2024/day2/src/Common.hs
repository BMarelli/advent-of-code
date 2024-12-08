module Common where

type Level = Int

lt :: Level -> Level -> Bool
lt a b =
  let distance = abs (a - b)
   in a < b && 1 <= distance && distance <= 3

gt :: Level -> Level -> Bool
gt = flip lt

type Report = [Level]
type Input = [Report]

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail
