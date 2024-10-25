module Common where

import qualified Data.Map.Strict as M

type Card = Int
type Hand = [Card]
type Bid = Int

data Game = G {hand :: Hand, bid :: Bid} deriving (Eq, Show)

type Input = [Game]

data Kind
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOAK
  | FullHouse
  | FourOAK
  | FiveOAK
  deriving (Eq, Ord, Show)

hand2kind :: M.Map Hand Kind
hand2kind =
  M.fromList
    [ ([1, 1, 1, 1, 1], HighCard)
    , ([2, 1, 1, 1], OnePair)
    , ([2, 2, 1], TwoPair)
    , ([3, 1, 1], ThreeOAK)
    , ([3, 2], FullHouse)
    , ([4, 1], FourOAK)
    , ([5], FiveOAK)
    ]
