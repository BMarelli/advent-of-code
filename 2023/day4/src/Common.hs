module Common where

import qualified Data.Set as S

-- | Representation of a scratchcard.
data Scratchcard = Scratchcard
  { _id :: Int
  , _winners :: S.Set Int
  , _num :: [Int]
  }
  deriving (Show, Eq)

type Input = [Scratchcard]

myWinners :: Scratchcard -> S.Set Int
myWinners sc = S.intersection (_winners sc) (S.fromList (_num sc))

cntWinners :: Scratchcard -> Int
cntWinners = S.size . myWinners
