module Main where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Common
import Parse

prepare :: String -> Input
prepare = mapMaybe (match scratchCard) . lines

solve1 :: Input -> Int
solve1 = sum . map (cardPoints . S.size . myWinners)
  where
    cardPoints :: Int -> Int
    cardPoints 0 = 0
    cardPoints n = 2 ^ (n - 1)

main :: IO ()
main = readFile "test/input/test.txt" >>= print . solve1 . prepare
