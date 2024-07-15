module Main where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Common
import Parse

prepare :: String -> Input
prepare = mapMaybe (match scratchCard) . lines

solve1 :: Input -> Int
solve1 = sum . map (series . S.size . myWinners)
  where
    series :: Int -> Int
    series 0 = 0
    series n = 2 ^ (n - 1)

main :: IO ()
main = readFile "test/input/test.txt" >>= print . solve1 . prepare
