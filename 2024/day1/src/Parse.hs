module Parse (input, match) where

import Common (Input)
import Control.Monad (void)
import Text.Regex.Applicative (RE, many, match, psym)
import Text.Regex.Applicative.Common (decimal)

type Parser a = RE Char a

space :: Parser ()
space = void $ many (psym (== ' '))

blank :: Parser ()
blank = void $ many (psym (== '\n'))

line :: Parser (Int, Int)
line = (,) <$> decimal <* space <*> decimal <* blank

input :: Parser Input
input = unzip <$> many line
