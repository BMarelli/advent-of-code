module Parse (match, parse, multiplication) where

import Common (Multiplication (..))
import Data.List (unfoldr)
import Text.Regex.Applicative (RE, findFirstInfix, match, string)
import Text.Regex.Applicative.Common (decimal)

type Parser a = RE Char a

multiplication :: Parser Multiplication
multiplication = string "mul(" *> (M <$> decimal <* string "," <*> decimal) <* string ")"

parse :: String -> [Multiplication]
parse = unfoldr (fmap (\(_, x, r) -> (x, r)) . findFirstInfix multiplication)
