{-# LANGUAGE InstanceSigs #-}
module Game where

data Game = Game {r, g, b :: Int}

instance Semigroup Game where
  (<>) :: Game -> Game -> Game
  Game r1 g1 b1 <> Game r2 g2 b2 = Game (max r1 r2) (max g1 g2) (max b1 b2)

instance Monoid Game where
  mempty :: Game
  mempty = Game 0 0 0