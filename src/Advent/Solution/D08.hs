module Advent.Solution.D08 where

import Advent.Prelude
import Control.Arrow
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (List, sort, sortBy)
import Data.Ord
import Text.ParserCombinators.ReadP

data V3 a = V3 !a !a !a deriving (Show, Eq, Ord)

instance (Num a) => Num (V3 a) where
  V3 x1 y1 z1 - V3 x2 y2 z2 = V3 (x1 - x2) (y1 - y2) (z1 - z2)

squaredNorm :: (Num a) => V3 a -> a
squaredNorm (V3 x y z) = x ^ 2 + y ^ 2 + z ^ 2

euclideanDistance :: V3 Int -> V3 Int -> Double
euclideanDistance u v = sqrt . fromIntegral $ squaredNorm (u - v)

parse :: String -> List (V3 Int)
parse = fmap parseLine . lines
  where
    parseLine =
      unsafeParse
        ( V3
            <$> number
            <* comma
            <*> number
            <* comma
            <*> number
        )
    comma = char ','
    number = readS_to_P reads

type Label = Int

type DisjointSets = IntMap Int

empty :: DisjointSets
empty = IntMap.empty

union :: Label -> Label -> DisjointSets -> DisjointSets
union a b m = link (find a m) (find b m)
  where
    link r s
      | r < s = IntMap.insert s r m
      | s < r = link s r
      | otherwise = m

-- | TODO: path compression
find :: Label -> DisjointSets -> Label
find l m = case IntMap.lookup l m of
  Just l' -> find l' m
  Nothing -> l

components :: DisjointSets -> List Label -> List (List Label)
components m =
  toList
    . IntMap.fromListWith (++)
    . map (\l -> (find l m, [l]))

solve1 junctionBoxes =
  product
    . take 3
    . sortBy (comparing Down)
    . map length
    . flip components labels
    . foldl' (\z (((i, _), (j, _))) -> union i j z) empty
    . take 1000
    $ pairs
  where
    (labels, pairs) = sortedPairs junctionBoxes

sortedPairs :: List (V3 Int) -> (List Int, List (((Int, V3 Int), (Int, V3 Int))))
sortedPairs junctionBoxes = (labels, pairs)
  where
    labeled = zip [1 ..] junctionBoxes
    labels = fst <$> labeled
    pairs =
      map snd $
        sort
          [ (d, ((i, u), (j, v)))
          | (i, u) <- labeled,
            (j, v) <- labeled,
            i < j,
            let d = euclideanDistance u v
          ]

solve2 junctionBoxes =
  (\((_, V3 x1 _ _), (_, V3 x2 _ _)) -> x1 * x2)
    . fst
    . last
    . takeWhile ((> 1) . length . flip components labels . snd)
    . zip pairs
    . scanl (\z ((i, _), (j, _)) -> union i j z) empty
    $ pairs
  where
    (labels, pairs) = sortedPairs junctionBoxes

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
