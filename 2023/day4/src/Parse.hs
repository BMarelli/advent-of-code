module Parse (
  scratchCard,
  match
) where

import Text.Regex.Applicative (RE, psym, string, match)
import Data.Char (isDigit)
import Control.Applicative
import qualified Data.Set as S
import Common

type Parser a = RE Char a

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

number :: Parser Int
number = read <$> many (psym isDigit)

blanks :: Parser ()
blanks = many (string " ") *> pure ()

iD :: Parser Int
iD = string "Card" *> blanks *> number <* string ":" <* blanks

scratchCard :: Parser Scratchcard
scratchCard = Scratchcard <$> iD <*> winners <* string " | " <* blanks <*> numbers
  where
    winners = S.fromList <$> sepBy number blanks
    numbers = sepBy number blanks
