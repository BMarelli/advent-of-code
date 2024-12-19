{-# LANGUAGE ApplicativeDo #-}

module Parse (match, parse) where

import Common (Input (..), Rule, Rules, Update, pair)
import Control.Applicative
import Control.Monad (void)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Regex.Applicative (RE, match, psym)
import Text.Regex.Applicative.Common (decimal)

type Parser a = RE Char a

sepBy :: (Alternative f) => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

blank :: Parser ()
blank = void $ many (psym (== '\n'))

rule :: Parser Rule
rule = (,) <$> decimal <* psym (== '|') <*> decimal <* blank

update :: Parser Update
update = sepBy decimal (psym (== ',')) <* blank

rules :: Parser Rules
rules = M.fromListWith S.union . map (pair snd (S.singleton . fst)) <$> many rule

parse :: Parser Input
parse = I <$> rules <* blank <*> many update
