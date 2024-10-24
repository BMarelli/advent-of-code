module Parse (match, races, race) where

import Common (Input, Milimeters (..), Miliseconds (..), Race (..))
import Control.Applicative
import Control.Monad (void)
import Data.Char (isDigit)
import Text.Regex.Applicative (RE, match, psym, string)

type Parser a = RE Char a

number :: Parser Int
number = read <$> some (psym isDigit)

spaces :: Parser ()
spaces = void $ many (string " ")

blank :: Parser ()
blank = void $ many (string "\n")

sepBy :: (Alternative f) => f a -> f b -> f [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

times :: Parser [Miliseconds]
times = string "Time:" *> spaces *> sepBy (Ms <$> number) spaces <* blank

distances :: Parser [Milimeters]
distances = string "Distance:" *> spaces *> sepBy (Mm <$> number) spaces <* blank

races :: Parser Input
races = zipWith R <$> times <*> distances
