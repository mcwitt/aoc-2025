module Main where

import AoC.Prelude
import Control.Arrow ((&&&), (>>>))
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.ParserCombinators.ReadP

parser :: ReadP ([(Int, Int)], [Int])
parser =
  (,)
    <$> (range `sepBy1` char '\n')
    <* char '\n'
    <*> (readS_to_P reads `sepBy1` char '\n')
    <* char '\n'
  where
    range = (,) <$> readS_to_P reads <* char '-' <*> readS_to_P reads

parse = unsafeParse parser

solve1 (ranges, ingredients) =
  length $
    filter (\ingredient -> any (\(a, b) -> a <= ingredient && ingredient <= b) ranges) $
      ingredients

solve2 (ranges, _) = fst $ foldl' addRange (0, 0) (sortBy (comparing fst) ranges)
  where
    addRange (size, bmax) (a, b) =
      let bmax' = max (b + 1) bmax
       in ( size + bmax' - max a bmax,
            bmax'
          )

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
