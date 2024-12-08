module Main where

import Common (Input, Level (..), Report, gt, lt, pairs)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Parse (match, parse)

prepare :: String -> Input
prepare = fromMaybe (error "wrong input!") . match parse

isSafe :: (Level -> Level -> Bool) -> Report -> Bool
isSafe op = all (uncurry op) . pairs

countSafe :: (Level -> Level -> Bool) -> Input -> Int
countSafe op = length . filter id . map (isSafe op)

solve1 :: Input -> Int
solve1 = uncurry (+) . (countSafe lt &&& countSafe gt)

solve2 :: Input -> Int
solve2 = const 0

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
