module Common where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M

data Range = Range {_source, _destination, _len :: Int}

mapping :: Range -> Int -> Int
mapping (Range s d l) n
  | n < s + l = d + n - s
  | otherwise = n

type Transformation = IM.IntMap (Int -> Int)

data Page = Page {_from, _to :: String, _transform :: Transformation}

type Pages = M.Map String Page

data Almanac = Almanac {_seeds :: [Int], _pages :: Pages}

type Input = Almanac
