{-# LANGUAGE TupleSections #-}

module Main where

import Common (Input, each)
import Control.Arrow ((&&&))
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Parse (input, match)

prepare :: String -> Input
prepare = fromMaybe (error "wrong input!") . match input

solve1 :: Input -> Int
solve1 = sum . map abs . uncurry (zipWith (-)) . each sort

frequencies :: (Ord a) => [a] -> M.Map a Int
frequencies = M.fromListWith (+) . map (,1)

solve2 :: Input -> Int
solve2 = sum . score . each frequencies
 where
  value :: M.Map Int Int -> (Int, Int) -> Int
  value m (k, v) = k * v * M.findWithDefault 0 k m
  score :: (M.Map Int Int, M.Map Int Int) -> [Int]
  score (ls, rs) = map (value ls) (M.assocs rs)

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
