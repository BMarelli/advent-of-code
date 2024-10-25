module Parse (match, games, game, pbid, phand, card) where

import Common (Bid, Card, Game (..), Hand, Input)
import Control.Applicative
import Control.Monad (void)
import Data.Char (isDigit)
import Text.Regex.Applicative (RE, match, psym, string)

type Parser a = RE Char a

number :: Parser Int
number = read <$> some (psym isDigit)

spaces :: Parser ()
spaces = void $ many $ psym (== ' ')

blank :: Parser ()
blank = void $ many (string "\n")

card :: Parser Card
card = card2value <$> psym (`elem` "23456789TJQKA")
 where
  card2value 'A' = 14
  card2value 'K' = 13
  card2value 'Q' = 12
  card2value 'J' = 11
  card2value 'T' = 10
  card2value c = read [c]

pbid :: Parser Bid
pbid = number

phand :: Parser Hand
phand = some card

game :: Parser Game
game = G <$> (phand <* spaces) <*> pbid <* blank

games :: Parser Input
games = many game
