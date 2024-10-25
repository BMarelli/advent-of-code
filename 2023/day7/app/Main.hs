module Main where

import Common (Game (..), Hand, Input, Kind, hand2kind)
import Control.Arrow ((&&&))
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Parse (games, match)

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g x = (f x, g x)

prepare :: String -> Input
prepare = fromMaybe (error "wrong input!") . match games

frequencies :: Hand -> M.Map Int Int
frequencies = M.fromListWith (+) . flip zip (repeat 1)

kind :: Hand -> Kind
kind = (hand2kind M.!) . sortOn negate . M.elems . frequencies

solve1 :: Input -> Int
solve1 = sum . zipWith (*) [1 ..] . M.elems . M.fromList . map entry
 where
  entry = pair (pair (kind . hand) hand) bid

solve2 :: Input -> ()
solve2 = const ()

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
