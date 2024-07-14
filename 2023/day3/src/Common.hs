module Common (
  Input,
  Coord(..),
  Located(..),
  Length(..),
  Token(..),
  neighbors,
  getLocations
) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Trans.Writer (execWriter, Writer, tell)
import Data.Foldable (traverse_)
import Control.Monad (foldM_)


type Input = [[Length Token]]

data Coord = Coord { _x :: Int, _y :: Int } deriving (Show, Eq, Ord)

data Located a = Located (S.Set Coord) a deriving (Show, Eq, Ord)

data Length a = Length Int a deriving (Show, Eq, Ord)

data Token = Empty | TokenInt Int | TokenChar Char deriving (Show, Eq, Ord)

neighbors :: Coord -> S.Set Coord
neighbors (Coord x y) = S.fromList $ do
  xs <- [-1..1]
  ys <- [-1..1]
  pure $ Coord (x + xs) (y + ys)

getLocations :: Input -> (S.Set (Located Int), M.Map Coord Char)
getLocations = execWriter . traverse_ go . zip [0..]
  where
    go :: (Int, [Length Token]) -> Writer (S.Set (Located Int), M.Map Coord Char) ()
    go (y, xs) = foldM_ included 0 xs
      where
        included :: Int -> Length Token -> Writer (S.Set (Located Int), M.Map Coord Char) Int
        included x (Length n t) = do
          case t of
            Empty -> pure ()
            TokenChar c -> tell (mempty, M.singleton (Coord x y) c)
            TokenInt i -> tell (S.singleton (Located (coords n x y) i), mempty)
          pure (x + n)
        coords :: Int -> Int -> Int -> S.Set Coord
        coords n' x' y' = S.fromList $ do
          xs' <- [0..n'-1]
          pure $ Coord (x' + xs') y'
