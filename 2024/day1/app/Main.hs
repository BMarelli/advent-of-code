{-# LANGUAGE TupleSections #-}

module Main where

import Common (Input, each)
import Control.Arrow ((&&&))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Parse (input, match)

prepare :: String -> Input
prepare = fromMaybe (error "wrong input!") . match input

solve1 :: Input -> Int
solve1 = sum . map abs . uncurry (zipWith (-)) . each sort

solve2 :: Input -> Int
solve2 = const 0

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
