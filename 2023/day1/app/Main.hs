module Main (main) where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (tails, isPrefixOf)
import Data.Maybe (mapMaybe)

type Input = [String]

parse :: String -> Input
parse = lines

sumValues :: [Int] -> Int
sumValues = (+) <$> (* 10) . head <*> last

valid :: [(String, Int)]
valid =
  [(pure (intToDigit x), x) | x <- [0 .. 9]]
    <> [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5),
        ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

solve1 :: Input -> Int
solve1 = sum . map (sumValues . map digitToInt . filter isDigit)

solve2 :: Input -> Int
solve2 = sum . map (sumValues . (mapMaybe digit . tails))
  where
    digit :: String -> Maybe Int
    digit s = foldr (\(k, v) acc -> if k `isPrefixOf` s then Just v else acc) Nothing valid

main :: IO ()
main = do
  f <- readFile "test/input/input.txt"
  (print . solve1 . parse) f
  (print . solve2 . parse) f
