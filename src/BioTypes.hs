module BioTypes
    ( DNABase(..)
    , RNABase(..)
    , AminoAcid(..)
    ) where

import Data.Ix (Ix)
import Data.List (find)
import Data.Maybe (fromJust)

data DNABase = A | T | C | G
  deriving (Eq, Ord, Enum, Bounded, Ix, Show, Read)
data RNABase = A' | U' | C' | G'
  deriving (Eq, Ord, Enum, Bounded, Ix)
data AminoAcid = A'' | R'' | N'' | D'' | C'' | Q'' | E'' | G'' | H'' | I'' | L'' | K'' | M'' | F'' | P'' | S'' | T'' | W'' | Y'' | V'' | B'' | Z'' | X'' | Star''
  deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show RNABase where
  show = fromJust . flip lookup [(A', "A"), (U', "U"), (C', "C"), (G', "G")]

instance Read RNABase where
  readsPrec _ []     = []
  readsPrec _ (c:cs) = case found of
    Just a  -> return (a,cs)
    Nothing -> []
    where found = find ((==c) . head . show) [minBound..maxBound]

instance Show AminoAcid where
  show = fromJust . flip lookup [(A'', "A"), (R'', "R"), (N'', "N"), (D'', "D"), (C'', "C"), (Q'', "Q"), (E'', "E"), (G'', "G"), (H'', "H"), (I'', "I"), (L'', "L"), (K'', "K"), (M'', "M"), (F'', "F"), (P'', "P"), (S'', "S"), (T'', "T"), (W'', "W"), (Y'', "Y"), (V'', "V"), (B'', "B"), (Z'', "Z"), (X'', "X"), (Star'',"*")]

instance Read AminoAcid where
  readsPrec _ []       = []
  readsPrec _ ('*':cs) = return (Star'', cs)
  readsPrec _ (c:cs)   = case found of
    Just a  -> return (a,cs)
    Nothing -> []
    where found = find ((==c) . head . show ) [minBound..maxBound]
