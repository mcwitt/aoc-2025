module Main where

import Control.Arrow ((&&&), (>>>))
import Data.MemoTrie (memoFix)
import Data.Nat (Nat (..))
import Data.Ord

type Digits = [Char]

parse = lines

joltages :: Nat -> Digits -> [Digits]
joltages Z _ = [[]]
joltages _ [] = []
joltages (S n) (x : xs) = ((x :) <$> joltages n xs) `mergeDown` joltages (S n) xs
  where
    mergeDown = mergeBy (comparing Down)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp = go
  where
    go xs [] = xs
    go [] ys = ys
    go (x : xs) (y : ys)
      | x `cmp` y == LT = x : go xs (y : ys)
      | otherwise = y : go (x : xs) ys

solve1 = sum . map (read . head . joltages 2)

joltages' :: Nat -> Digits -> [Digits]
joltages' = curry (memoFix f)
  where
    f _ (Z, _) = [[]]
    f _ (_, []) = []
    f k (S n, x : xs) = ((x :) <$> k (n, xs)) `mergeDown` k (S n, xs)
      where
        mergeDown = mergeBy (comparing Down)

solve2 = sum . map (read . head . joltages' 12)

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
