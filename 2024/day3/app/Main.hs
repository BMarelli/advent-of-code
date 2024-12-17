module Main where

import Common (Input, Instruction (..), apply)
import Control.Arrow ((&&&))
import Data.List (mapAccumL)
import Parse (instruction, multiplication, parse)

prepare :: String -> Input
prepare = id

solve1 :: Input -> Int
solve1 = sum . map apply . parse multiplication

solve2 :: Input -> Int
solve2 = sum . snd . mapAccumL handleInstructions True . parse instruction
 where
  handleInstructions _ Do = (True, 0)
  handleInstructions _ Dont = (False, 0)
  handleInstructions s (V m) = (s, if s then apply m else 0)

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
