module Main (main) where

type Input = [String]


parse :: String -> Input
parse = lines

solve :: Input -> Int
solve = undefined

main :: IO ()
main = readFile "test/input/test.txt" >>= print . solve . parse
