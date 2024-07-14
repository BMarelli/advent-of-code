module Parse (line, (=~)) where

import Common (Length (..), Token (..))
import Data.Char (isDigit)
import Text.Regex.Applicative (RE, psym, some, Alternative (many), (=~))
import Data.Foldable (asum)

type Parser a = RE Char a

number :: Parser (Length Token)
number = (\n -> Length (length n) . TokenInt . read $ n) <$> some (psym isDigit)

blank :: Parser (Length Token)
blank = Length 1 Empty <$ psym (== '.')

symbol :: Parser (Length Token)
symbol = Length 1 . TokenChar <$> psym (not . ((||) <$> isDigit <*> (== '.')))

line :: Parser [Length Token]
line = many (asum [number, blank, symbol])
