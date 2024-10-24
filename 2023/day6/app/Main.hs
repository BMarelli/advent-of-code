module Main where

import Common (Input, Milimeters (..), Miliseconds (..), Race (..))
import Control.Arrow ((&&&), (***))
import Data.Maybe (fromMaybe)
import Parse (match, race, races)

prepare :: String -> (Input, Race)
prepare = match' races &&& match' race
 where
  match' p = fromMaybe (error "wrong input!") . match p

calculateWays :: Race -> Int
calculateWays (R (Ms t) (Mm d)) = t - (2 * ceiling record) + 1
 where
  record = (t' - sqrt (t' * t' - 4 * d')) / 2
  t' = fromIntegral t
  d' = fromIntegral d

solve1 :: Input -> Int
solve1 = product . map calculateWays

solve2 :: Race -> Int
solve2 = calculateWays

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 *** solve2) . prepare
