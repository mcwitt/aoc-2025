{-# LANGUAGE ParallelListComp #-}

module Advent.Solution.D06 where

import Control.Arrow ((&&&), (>>>))
import Data.Char (isSpace)
import Data.List (transpose)

parse = lines

solve1 =
  sum
    . map
      ( \xs ->
          let args = read <$> init xs
              op = head (last xs)
           in foldr1 (eval op) args
      )
    . transpose
    . map words

type Op = Char

eval :: Op -> Int -> Int -> Int
eval '+' a b = a + b
eval '*' a b = a * b
eval op _ _ = error ("unrecognized op: '" ++ [op] ++ "'")

solve2 xss =
  let argss = map read <$> splitOn (all isSpace) (transpose $ init xss)
      ops = filter (not . isSpace) (last xss)
   in sum [foldr1 (eval op) args | args <- argss | op <- ops]

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p xs = case dropWhile p xs of
  [] -> []
  xs' -> ys : splitOn p xs''
    where
      (ys, xs'') = break p xs'

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
