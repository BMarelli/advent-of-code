module Main where

import Common (Input, Multiplication (..), apply)
import Control.Arrow ((&&&))
import Parse (parse)

prepare :: String -> Input
prepare = id

solve1 :: Input -> Int
solve1 = sum . map apply . parse

solve2 :: Input -> Int
solve2 = const 0

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
