module Advent.Solution.D01 where

import Advent.Prelude
import Control.Arrow ((&&&), (>>>))
import Data.DivMod
import Data.Functor ((<&>))
import Data.Mod
import Text.ParserCombinators.ReadP

parser :: ReadP Int
parser = l <++ r
  where
    l = char 'L' *> readS_to_P reads <&> negate
    r = char 'R' *> readS_to_P reads

parse :: String -> [Int]
parse = map (unsafeParse parser) . lines

solve1 :: [Int] -> Int
solve1 = length . filter ((== 0) . unMod) . scanl (+) (mkMod 50) . map (mkMod @100)

solve2 :: [Int] -> Int
solve2 xs = sum $ zipWith f sums (tail sums)
  where
    sums :: [DivMod 100]
    sums = scanl (+) 50 $ map fromIntegral xs

    f (DivMod q1 0) (DivMod q2 _) | q2 < q1 = abs (q1 - q2) - 1 -- turn left from zero
    f (DivMod q1 _) (DivMod q2 0) | q2 <= q1 = abs (q1 - q2) + 1 -- turn left to zero
    f (DivMod q1 _) (DivMod q2 _) = abs (q1 - q2)

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
