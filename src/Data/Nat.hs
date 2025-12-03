{-# LANGUAGE TypeFamilies #-}

module Data.Nat where

import Data.MemoTrie
import GHC.Generics (Generic)

data Nat = Z | S Nat deriving (Show, Eq, Generic)

instance Num Nat where
  fromInteger 0 = Z
  fromInteger n = S (fromInteger (pred n))

instance HasTrie Nat where
  newtype Nat :->: b = NatTrie {unNatTrie :: Reg Nat :->: b}
  trie = trieGeneric NatTrie
  untrie = untrieGeneric unNatTrie
  enumerate = enumerateGeneric unNatTrie
