module Main (main) where

import Data.Char (isAlpha, isDigit)
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Game

type Input = [String]

colors :: [(String, Int)]
colors = [("red", 12), ("green", 13), ("blue", 14)]

parse :: String -> Input
parse = lines

filterSpecialChars :: (Int, String) -> (Int, [String])
filterSpecialChars = fst &&& drop 2 . words . mapMaybe isValid . snd
  where
    isValid :: Char -> Maybe Char
    isValid ';' = Nothing
    isValid ':' = Nothing
    isValid ',' = Nothing
    isValid c = Just c

zipper :: [String] -> [(String, Int)]
zipper s = zip (filter (all isAlpha) s) (map read $ filter (all isDigit) s)

checkValidId :: (Int, [String]) -> Maybe Int
checkValidId (i, ss) = if all check $ zipper ss then Just i else  Nothing
    where
        check :: (String, Int) -> Bool
        check = (>=) <$> (flip lookup colors . fst) <*> Just . snd

solve1 :: Input -> Int
solve1 = sum . mapMaybe (checkValidId . filterSpecialChars) . zip [1..]

calculateMin :: (Int, [String]) -> Int
calculateMin = prodGame . foldl (<>) mempty . map toGame . zipper . snd
    where
        toGame :: (String, Int) -> Game
        toGame ("red", n) = Game n 0 0
        toGame ("green", n) = Game 0 n 0
        toGame ("blue", n) = Game 0 0 n
        toGame _ = mempty
        prodGame :: Game -> Int
        prodGame p = r p * g p * b p

solve2 :: Input -> Int
solve2 = sum . zipWith (curry (calculateMin . filterSpecialChars)) [1..]

main :: IO ()
main = readFile "test/input/input.txt" >>= print . (solve1 &&& solve2) . parse
