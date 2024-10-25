module Main where

import Common
import Control.Arrow ((&&&))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Parse (almanac, match)

source2destination :: Int -> Transformation -> Int
source2destination n = maybe n (apply . snd) . IM.lookupLT n
 where
  apply :: (Int -> Int) -> Int
  apply f = f n

prepare :: String -> Input
prepare = fromMaybe (error "wrong input!") . match almanac

solve1 :: Input -> Int
solve1 a = minimum . map (go "seed" (_pages a)) . _seeds $ a
 where
  go :: String -> Pages -> Int -> Int
  go s ps n = maybe n search . M.lookup s $ ps
   where
    search :: Page -> Int
    search (Page _ t m) = go t ps . source2destination n $ m

solve2 :: Input -> Int
solve2 = length . _seeds

main :: IO ()
main = readFile "test/input/test.txt" >>= print . (solve1 &&& solve2) . prepare
