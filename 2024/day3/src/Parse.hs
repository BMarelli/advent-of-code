module Parse (match, parse, multiplication, instruction) where

import Common (Instruction (..), Multiplication (..))
import Data.Functor (($>))
import Data.List (unfoldr)
import Text.Regex.Applicative (RE, findFirstInfix, match, string, (<|>))
import Text.Regex.Applicative.Common (decimal)

type Parser a = RE Char a

multiplication :: Parser Multiplication
multiplication = string "mul(" *> (M <$> decimal <* string "," <*> decimal) <* string ")"

instruction :: Parser (Instruction Multiplication)
instruction =
  string "do()"
    $> Do
      <|> (string "don't()" $> Dont)
      <|> (V <$> multiplication)

parse :: Parser a -> String -> [a]
parse p = unfoldr (fmap (\(_, x, r) -> (x, r)) . findFirstInfix p)
