module Parse (parse, match) where

import Common (Input, Level, Report)
import Control.Applicative (Alternative)
import Control.Monad (void)
import Text.Regex.Applicative (RE, many, match, psym)
import Text.Regex.Applicative.Common (decimal)

type Parser a = RE Char a

sepBy :: (Alternative f) => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

space :: Parser ()
space = void $ many (psym (== ' '))

blank :: Parser ()
blank = void $ many (psym (== '\n'))

level :: Parser Level
level = decimal

report :: Parser Report
report = level `sepBy` space <* blank

parse :: Parser Input
parse = many report
