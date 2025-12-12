module Advent.Solution.D04 where

import Control.Arrow ((&&&), (>>>))
import Control.Comonad.Store
import Data.List (unfoldr)
import Data.Set (Set)
import Data.Set qualified as Set

type Coord = (Int, Int)

type Grid = Store Coord

parse :: String -> ((Int, Int), Set Coord)
parse s =
  let xss = lines s
   in ( (length xss, length (head xss)),
        Set.fromList
          [ (r, c)
          | (r, xs) <- zip [1 ..] xss,
            (c, '@') <- zip [1 ..] xs
          ]
      )

solve1 (dims, coords) =
  let grid = store (`Set.member` coords) (0, 0)
      accessible = extend rule grid
   in length $ filter (\c -> peek c grid && peek c accessible) $ allCoords dims

allCoords (h, w) = [(r, c) | r <- [1 .. h], c <- [1 .. w]]

rule :: Grid Bool -> Bool
rule = (< 5) . length . filter id . experiment neighborhood
  where
    neighborhood (r, c) = [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1]]

step :: (Int, Int) -> Set Coord -> Maybe (Int, Set Coord)
step dims coords =
  let grid = store (`Set.member` coords) (0, 0)
      accessible = extend rule grid
      nextCoords = Set.fromList $ filter (\c -> peek c grid && not (peek c accessible)) $ allCoords dims
      numRemoved = length coords - length nextCoords
   in if numRemoved == 0
        then Nothing
        else Just (numRemoved, nextCoords)

solve2 (dims, grid) = sum $ unfoldr (step dims) grid

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
