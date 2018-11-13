{-# LANGUAGE FlexibleInstances #-}
module BioArbitrary
  (
  ) where

import qualified Data.List as L
import Data.Maybe (fromJust)
import Test.QuickCheck

import BioTypes (DNABase, RNABase)

finiteArbitrary :: (Enum a, Bounded a) => Gen a
finiteArbitrary = elements [minBound..]

finiteEqCoArbitrary :: (Enum a, Bounded a, Eq a) => a -> Gen b -> Gen b
finiteEqCoArbitrary = variant . fromJust . flip L.elemIndex [minBound..]

instance Arbitrary DNABase where
  arbitrary = finiteArbitrary

instance CoArbitrary DNABase where
  coarbitrary = finiteEqCoArbitrary

instance Arbitrary RNABase where
  arbitrary = finiteArbitrary

instance CoArbitrary RNABase where
  coarbitrary = finiteEqCoArbitrary
