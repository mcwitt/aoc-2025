{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.DivMod (DivMod, pattern DivMod) where

import Data.Proxy
import GHC.TypeNats hiding (Mod)

data DivMod (k :: Nat) = DivMod Int Int deriving (Eq, Ord, Show)

mkDivMod :: forall k. (KnownNat k) => Int -> DivMod k
mkDivMod m = DivMod q r
  where
    kVal = fromIntegral (natVal (Proxy @k))
    (q, r) = m `divMod` kVal

unDivMod :: forall k. (KnownNat k) => DivMod k -> Int
unDivMod (DivMod q r) = q * fromIntegral (natVal (Proxy @k)) + r

liftOp :: (KnownNat k) => (Int -> Int) -> DivMod k -> DivMod k
liftOp f = mkDivMod . f . unDivMod

liftOp2 op x1 x2 = mkDivMod (unDivMod x1 `op` unDivMod x2)

instance (KnownNat k) => Num (DivMod k) where
  (+) = liftOp2 (+)
  (-) = liftOp2 (-)
  (*) = liftOp2 (*)
  negate = liftOp negate
  abs = liftOp abs
  signum = liftOp signum
  fromInteger = mkDivMod . fromIntegral

instance (KnownNat k) => Enum (DivMod k) where
  toEnum = mkDivMod
  fromEnum = unDivMod

instance (KnownNat k) => Real (DivMod k) where
  toRational = toRational . unDivMod

instance (KnownNat k) => Integral (DivMod k) where
  toInteger = fromIntegral . unDivMod

  quotRem a b =
    let (q, r) = quotRem (unDivMod a) (unDivMod b)
     in (mkDivMod q, mkDivMod r)
