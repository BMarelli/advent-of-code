module Main where

import Common (Input (..), Rules, Update)
import Control.Arrow ((&&&))
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Parse (match, parse)

prepare :: String -> Input
prepare = fromMaybe (error "Failed to parse input") . match parse

getMiddle :: Update -> Int
getMiddle xs = xs !! (length xs `div` 2)

correct :: Rules -> Update -> Bool
correct _ [] = True
correct rs (x : xs)
  | any (`S.member` M.findWithDefault mempty x rs) xs = False
  | otherwise = correct rs xs

solve1 :: Input -> Int
solve1 i = sum . map getMiddle . filter (correct (_rules i)) . _updates $ i

solve2 :: Input -> Int
solve2 i = sum . map (getMiddle . orderIncorrect) . filter (not . correct rules) . _updates $ i
 where
  rules = _rules i
  compareUpdate :: Int -> Int -> Ordering
  compareUpdate x y
    | S.member y (M.findWithDefault mempty x rules) = LT
    | S.member x (M.findWithDefault mempty y rules) = GT
    | otherwise = EQ
  orderIncorrect :: Update -> Update
  orderIncorrect = sortBy compareUpdate

main :: IO ()
main = readFile "test/input.txt" >>= print . (solve1 &&& solve2) . prepare
