{-# LANGUAGE ParallelListComp #-}

module Advent.Solution.D03 where

import Control.Arrow ((&&&), (>>>))
import Data.Char (digitToInt)

type Digit = Int

parse = map (map digitToInt) . lines

-- Elegant solution stolen from Eric Mertens (including usage of
-- ParallelListComp as a prettier alternative to zipWith)

maxJoltages :: [Digit] -> [Int]
maxJoltages = foldl' addDigit (repeat 0)

addDigit :: [Int] -> Digit -> [Int]
addDigit prev d =
  [ max x1 (10 * x0 + d)
  | x0 <- 0 : prev
  | x1 <- prev
  ]

solve k = sum . map ((!! (k - 1)) . maxJoltages)

solve1 = solve 2

solve2 = solve 12

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
