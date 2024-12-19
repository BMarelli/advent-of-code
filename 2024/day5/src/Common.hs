module Common where

import qualified Data.Map as M
import qualified Data.Set as S

type Rule = (Int, Int)
type Rules = M.Map Int (S.Set Int)
type Update = [Int]

data Input = I {_rules :: Rules, _updates :: [Update]} deriving (Show)

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g = (,) <$> f <*> g
