module Advent.Prelude (unsafeParse) where

import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP

unsafeParse :: ReadP a -> String -> a
unsafeParse p s = case [a | (a, "") <- ReadP.readP_to_S p s] of
  a : _ -> a
  [] -> error "unsafeParse: failed to parse"
