module Advent.Solution.D07 where

import Control.Arrow ((&&&), (>>>))
import Control.Monad (foldM)
import Control.Monad.Writer (Writer, execWriter, runWriter, writer)
import Data.Foldable (Foldable (fold))
import Data.IntMap.Monoidal (MonoidalIntMap)
import Data.IntMap.Monoidal qualified as IntMap
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)
import Data.Monoid (Sum (getSum))

parse s =
  let (xs : xss) = lines s
   in (fromJust $ elemIndex 'S' xs, elemIndices '^' <$> xss)

solve1 (startPos, splitterRows) =
  getSum
    . execWriter
    . foldM (foldM (flip splitBeams)) (IntMap.singleton startPos 1)
    $ splitterRows

type Count = Sum Int

type Multiset = MonoidalIntMap Count

splitBeams :: Int -> Multiset -> Writer Count Multiset
splitBeams pos = fmap (IntMap.fromListWith (+) . concat) . traverse f . IntMap.assocs
  where
    f (p, m)
      | p == pos = writer ([(p - 1, m), (p + 1, m)], 1)
      | otherwise = pure [(p, m)]

solve2 (startPos, splitterRows) =
  getSum
    . fold
    . fst
    . runWriter
    . foldM (foldM (flip splitBeams)) (IntMap.singleton startPos 1)
    $ splitterRows

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
