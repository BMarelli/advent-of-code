module Main where

import Common (Input, Level (..), Report, gt, lt, pairs)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Parse (match, parse)

prepare :: String -> Input
prepare = fromMaybe (error "wrong input!") . match parse

isSafe :: (Level -> Level -> Bool) -> Report -> Bool
isSafe op = all (uncurry op) . pairs

solve1 :: Input -> Int
solve1 = uncurry (+) . (countSafe lt &&& countSafe gt)
 where
  countSafe :: (Level -> Level -> Bool) -> Input -> Int
  countSafe op = length . filter id . map (isSafe op)

solve2 :: Input -> Int
solve2 = uncurry (+) . (countSafe lt &&& countSafe gt)
 where
  countSafe :: (Level -> Level -> Bool) -> Input -> Int
  countSafe op = length . filter id . map (any (isSafe op) . dampener)
  dampener :: Report -> [Report]
  dampener xs = xs : [removeAt i xs | i <- [0 .. length xs - 1]]
  removeAt :: Int -> [a] -> [a]
  removeAt i xs = take i xs ++ drop (i + 1) xs

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
