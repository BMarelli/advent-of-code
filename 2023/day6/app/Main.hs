module Main where

import Common (Input, Milimeters (..), Miliseconds (..), Race (..))
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Parse (match, races)

prepare :: String -> Input
prepare = fromMaybe (error "wrong input!") . match races

calculateWays :: Race -> Int
calculateWays (R (Ms t) (Mm d)) = t - (2 * ceiling record) + 1
 where
  record = (t' - sqrt (t' * t' - 4 * d')) / 2
  t' = fromIntegral t
  d' = fromIntegral d

solve1 :: Input -> Int
solve1 = product . map calculateWays

solve2 :: Input -> Int
solve2 = length

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
