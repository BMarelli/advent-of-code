module Main where

import Common
import Control.Arrow ((&&&))
import qualified Data.IntMap.Lazy as M
import Data.Maybe (mapMaybe)
import Parse

prepare :: String -> Input
prepare = mapMaybe (match scratchCard) . lines

solve1 :: Input -> Int
solve1 = sum . map (cardPoints . cntWinners)
 where
  cardPoints :: Int -> Int
  cardPoints 0 = 0
  cardPoints n = 2 ^ (n - 1)

solve2 :: Input -> Int
solve2 sc = M.size cards + (sum . M.elems $ cards)
 where
  cards :: M.IntMap Int
  cards = M.fromList $ do
    c <- sc
    let cnt = cntWinners c
    let n = _id c
    pure (n, cnt + sum [(M.!) cards (n + i) | i <- [1 .. cnt]])

main :: IO ()
main = readFile "test/input/test.txt" >>= print . (solve1 &&& solve2) . prepare
