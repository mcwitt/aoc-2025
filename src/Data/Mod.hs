{-# LANGUAGE DataKinds #-}

module Data.Mod (Mod (unMod), mkMod) where

import Data.Proxy
import GHC.TypeNats hiding (Mod)

newtype Mod (k :: Nat) = Mod {unMod :: Int} deriving (Eq, Ord, Enum, Show)

mkMod :: forall k. (KnownNat k) => Int -> Mod k
mkMod m = Mod (m `mod` fromIntegral (natVal (Proxy @k)))

liftOp :: (KnownNat k) => (Int -> Int) -> Mod k -> Mod k
liftOp f (Mod m) = mkMod (f m)

liftOp2 op (Mod m) (Mod n) = mkMod (m `op` n)

instance (KnownNat k) => Num (Mod k) where
  (+) = liftOp2 (+)
  (-) = liftOp2 (-)
  (*) = liftOp2 (*)
  negate = liftOp negate
  abs = liftOp abs
  signum = liftOp signum
  fromInteger = mkMod . fromIntegral
