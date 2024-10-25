{-# LANGUAGE ApplicativeDo #-}

module Parse (
  match,
  almanac,
) where

import Common
import Control.Monad
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Text.Regex.Applicative

type Parser a = RE Char a

sepBy :: (Alternative f) => f a -> f b -> f [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

number :: Parser Int
number = read <$> many (psym isDigit)

blank :: Parser ()
blank = void $ many (psym (== '\n'))

space :: Parser ()
space = void $ many (psym (== ' '))

seed :: Parser [Int]
seed = string "seeds: " *> sepBy number space <* blank

index :: Parser (String, String)
index = do
  from <- many (psym (/= ' '))
  string "-to-"
  to <- many (psym (/= ' '))
  space
  string "map:"
  blank
  return (from, to)

range :: Parser Range
range = flip Range <$> number <* space <*> number <* space <*> number <* blank

transformation :: Parser Transformation
transformation = range2transformation <$> some range
 where
  range2transformation = IM.fromList . map go
  go r@(Range src _ _) = (src, mapping r)

page :: Parser Page
page = uncurry Page <$> index <*> transformation

pages :: Parser Pages
pages = M.fromList . map go <$> some page
 where
  go p = (_from p, p)

almanac :: Parser Almanac
almanac = Almanac <$> seed <* blank <*> pages
