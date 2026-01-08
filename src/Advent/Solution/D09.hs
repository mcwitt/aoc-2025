{-# LANGUAGE ParallelListComp #-}

module Advent.Solution.D09 where

import Advent.Prelude (unsafeParse)
import Control.Arrow ((&&&), (>>>))
import Data.Bifunctor
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Ix (inRange)
import Data.List
import Data.Maybe
import Data.Ord
import Text.ParserCombinators.ReadP hiding (between)

parse = map parsePoint . lines

data V2 a = V2 !a !a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative V2 where
  pure x = V2 x x
  V2 f g <*> V2 x y = V2 (f x) (g y)

type Point = V2 Int

parsePoint :: String -> Point
parsePoint = unsafeParse (V2 <$> number <* char ',' <*> number)
  where
    number = readS_to_P reads

area :: Point -> Point -> Int
area (V2 x1 y1) (V2 x2 y2) =
  let w = abs (x1 - x2) + 1
      h = abs (y1 - y2) + 1
   in w * h

solve1 :: List Point -> Int
solve1 ps = maximum (area <$> ps <*> ps)

solve2 :: List Point -> Int
solve2 ps =
  head
    [ area_
    | (area_, (cp1, cp2)) <- candidatesByAreaDesc,
      all isValidTile' (rectanglePoints cp1 cp2)
    ]
  where
    -- NOTE: pad compressed points to check the regions between borders
    -- (necessary for correctness)
    cps = map ((* 2) <$>) (compressPoints ps)

    isValidTile' = isValidTile cps
    candidatesByAreaDesc =
      sortBy
        (comparing Down)
        [ (area p1 p2, (cp1, cp2))
        | (p1, cp1) <- zip ps cps,
          (p2, cp2) <- zip ps cps,
          p1 /= p2
        ]

compress :: (Ord a) => List a -> List Int
compress xs =
  let to = flip zip [0 ..] . map head . group . sort $ xs
   in map (fromJust . flip lookup to) xs

compressPoints :: List Point -> List Point
compressPoints ps =
  let V2 xs ys = compress <$> sequenceA ps
   in zipWith V2 xs ys

data Tile = Red | Green deriving (Show)

isValidTile :: List Point -> Point -> Bool
isValidTile ps = \p -> go p
  where
    segments = mkSegment <$> zip ps (tail ps)
    xings = mkCrossings segments
    go p = onPath p segments || windingNumber p xings /= 0

-- | Return all points in the border and interior of a rectangle
-- given two corners. Works by iteratively bisecting the rectangle to
-- achieve approximately uniform sampling for all prefixes.
rectanglePoints :: Point -> Point -> List Point
rectanglePoints (V2 x1_ y1_) (V2 x2_ y2_) =
  go
    (V2 (min x1_ x2_) (min y1_ y2_))
    (V2 (max x1_ x2_) (max y1_ y2_))
  where
    go (V2 x1 y1) (V2 x2 y2)
      | x2 < x1 || y2 < y1 = []
      | x1 == x2 && y1 == y2 = [V2 x1 y1]
      | w >= h =
          interleave
            (go (V2 x1 y1) (V2 xm y2))
            (go (V2 (xm + 1) y1) (V2 x2 y2))
      | otherwise =
          interleave
            (go (V2 x1 y1) (V2 x2 ym))
            (go (V2 x1 (ym + 1)) (V2 x2 y2))
      where
        w = x2 - x1
        h = y2 - y1
        xm = x1 + w `div` 2
        ym = y1 + h `div` 2

interleave :: [a] -> [a] -> [a]
interleave (a : as) bs = a : interleave bs as
interleave [] bs = bs

data Segment = V !Int !Int !Int | H !Int !Int !Int deriving (Show)

mkSegment :: (Point, Point) -> Segment
mkSegment ((V2 x1 y1), (V2 x2 y2))
  | x1 == x2 = V x1 y1 y2
  | y1 == y2 = H y1 x1 x2
  | otherwise = error "diagonal segment"

type Crossings = IntMap (List (Int, Int))

mkCrossings :: List Segment -> Crossings
mkCrossings = IntMap.fromListWith (++) . map (second (: [])) . concatMap go
  where
    go (H y x1 x2)
      | x1 <= x2 = [(x, (y, 1)) | x <- [x1 .. x2]]
      | otherwise = [(x, (y, -1)) | x <- [x2 .. x1]]
    go _ = []

windingNumber :: Point -> Crossings -> Int
windingNumber (V2 x y) xings =
  sum
    [ parity
    | (y', parity) <- fromMaybe [] (IntMap.lookup x xings),
      y' > y
    ]

onPath :: Point -> List Segment -> Bool
onPath (V2 x y) = any onSegment
  where
    onSegment (V x' y1 y2) = x == x' && onSegment1 (y1, y2) y
    onSegment (H y' x1 x2) = y == y' && onSegment1 (x1, x2) x
    onSegment1 (z1, z2) = inRange (min z1 z2, max z1 z2)

main = interact (parse >>> (solve1 &&& solve2) >>> show)
