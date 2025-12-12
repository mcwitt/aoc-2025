{-# LANGUAGE ParallelListComp #-}

module Advent.Solution.D11 where

import Control.Arrow ((&&&), (>>>))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.MemoTrie (HasTrie, memoFix)

type Adj a = Map a [a]

parse :: String -> Adj String
parse = Map.fromList . map ((\(w : ws) -> (init w, ws)) . words) . lines

solve1 adj = countPaths (mkNext adj) "out" "you"

countPaths :: (Eq a, HasTrie a) => (a -> [a]) -> a -> a -> Int
countPaths next target = memoFix go
  where
    go k x
      | x == target = 1
      | otherwise = sum [k x' | x' <- next x]

mkNext :: (Ord a) => Adj a -> a -> [a]
mkNext adj = fromMaybe [] . (adj Map.!?)

solve2 adj =
  sum $
    map
      countPathsTour
      [ ["svr", "fft", "dac", "out"],
        ["svr", "dac", "fft", "out"]
      ]
  where
    countPathsTour waypoints =
      product
        [ countPaths (mkNext adj) b a
        | a <- waypoints
        | b <- tail waypoints
        ]

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
