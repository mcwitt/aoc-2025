module Advent.Solution.D02 where

import Advent.Prelude
import Control.Arrow ((&&&), (>>>))
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.ParserCombinators.ReadP

parser :: ReadP [(Int, Int)]
parser =
  ( (,)
      <$> readS_to_P reads
      <* char '-'
      <*> readS_to_P reads
  )
    `sepBy1` char ','
    <* char '\n'

parse = unsafeParse parser

mkInvalid :: Int -> Maybe Int
mkInvalid n =
  let s = show n
      q = length s `quot` 2
   in if take q s == drop q s then Just n else Nothing

solve1 :: [(Int, Int)] -> Int
solve1 = sum . concatMap (mapMaybe mkInvalid . uncurry enumFromTo)

mkInvalid2 :: Int -> Maybe Int
mkInvalid2 n =
  listToMaybe
    [ n
    | let s = show n,
      let l = length s,
      l >= 2,
      (_, q) <- nub (factors l),
      let c : cs = chunks q s,
      all (c ==) cs
    ]

factors :: Int -> [(Int, Int)]
factors = go 1
  where
    go m n =
      case [ (d, q)
           | d <- [2 .. floor (sqrt (fromIntegral n) :: Double)],
             let (q, r) = n `quotRem` d,
             r == 0
           ] of
        (d, q) : _ -> (d, q * m) : go (m * d) (n `div` d)
        [] -> [(n, m)]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks k xs = take k xs : chunks k (drop k xs)

solve2 :: [(Int, Int)] -> Int
solve2 = sum . concatMap (mapMaybe mkInvalid2 . uncurry enumFromTo)

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
