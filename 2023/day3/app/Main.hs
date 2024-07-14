module Main (main) where

import Parse (line, (=~))
import Common
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow ((&&&))


prepare :: String -> Input
prepare = mapMaybe (=~ line) . lines

solve1 :: String -> Int
solve1 = sum . go . getLocations . prepare
  where
    go :: (S.Set (Located Int), M.Map Coord Char) -> [Int]
    go (nums, sym) = mapMaybe values . S.toList $ nums
      where
        values :: Located Int -> Maybe Int
        values (Located c i) | S.null (S.intersection (M.keysSet sym) (foldMap neighbors c)) = Nothing
                             | otherwise = Just i

solve2 :: String -> Int
solve2 = sum . go . getLocations . prepare
  where
    go :: (S.Set (Located Int), M.Map Coord Char) -> [Int]
    go (nums, sym) = mapMaybe values . M.assocs $ sym
      where
        values :: (Coord, Char) -> Maybe Int
        values (c, '*') = case twoNeighbors c nums of
                            Just (a, b) -> Just (a * b)
                            Nothing -> Nothing
        values _ = Nothing
    twoNeighbors :: Coord -> S.Set (Located Int) -> Maybe (Int, Int)
    twoNeighbors c coords = case filter (\(Located c' _) -> S.member c (foldMap neighbors c')) . S.toList $ coords of
                              [Located _ a, Located _ b] -> Just (a, b)
                              _ -> Nothing 

main :: IO ()
main = readFile "test/input/test.txt" >>= print . (solve1 &&& solve2)
