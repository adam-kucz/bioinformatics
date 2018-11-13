{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Alignment
    ( Dir(..)
    , needlemanWunsch
    , showAlignment
    , showAlignedSequences
    ) where

import Data.Function (on)
import Data.Maybe (fromJust)

import Matrix (Matrix, fromList, (!), size, mmap)
import MatrixInteraction (stringifyLabeledMatrix)
import Util (onBoth, tApp, align)

data Dir = UP | LEFT | DIAG | END
  deriving (Eq, Ord)

instance Show Dir where
  show UP   = "↑"
  show LEFT = "←"
  show DIAG = "↖"
  show END  = " "

type AlignmentMat a = Matrix Int (a, Dir)

nwScoring :: forall a n. (Num n, Ord n) =>
  n -> (a -> a -> n) -> ((Int, Int) -> n) -> (Int, Int) -> (a,a) -> (n, Dir)
nwScoring d score f = value
  where value :: (Int, Int) -> (a,a) -> (n, Dir)
        value (1, 1) _ = (                        0, END)
        value (1, j) _ = (-(fromIntegral j - 1) * d, LEFT)
        value (i, 1) _ = (-(fromIntegral i - 1) * d, UP)
        value (i, j) (s1,s2) =
          maximum [(f (i-1,j) - d, UP),
                   (f (i,j-1) - d, LEFT),
                   (f (i-1,j-1) + score s1 s2, DIAG)] 

needlemanWunsch ::
  forall a n. (Show a, Num n, Ord n) =>
  [a] -> [a] -> n -> (a -> a -> n) -> AlignmentMat n
needlemanWunsch seq1 seq2 d score = mat
  where mat = mmap (nwScoring d score (fst . (mat !))) .
              fromJust .
              fromList $
              [[(s1, s2) | s2 <- undefined:seq2]
                         | s1 <- undefined:seq1]
              
showAlignment ::
  forall a n. (Show a, Show n) => [a] -> [a] -> AlignmentMat n -> [String]
showAlignment seq1 seq2 mat  =
  (stringifyLabeledMatrix "|" ' ' seq1 seq2 $ fst <$> mat) ++
  (stringifyLabeledMatrix  "" ' ' seq1 seq2 $ snd <$> mat)

showAlignedSequences ::
  forall a n. Show a => [a] -> [a] -> AlignmentMat n -> (String,String)
showAlignedSequences seq1 seq2 mat =
  reverse `onBoth` (go (size mat) `on` reverse) seq1 seq2
  where go :: (Int,Int) -> [a] -> [a] -> (String,String)
        go (i, j) s1 s2 =
          case snd $ mat ! (i, j) of
            END -> ("","")
            UP  -> (++) `onBoth`
                   alignPair (show $ head s1) "" `tApp`
                   go (i-1, j) (tail s1) s2
            LEFT  -> (++) `onBoth`
                   alignPair "" (show $ head s2) `tApp`
                   go (i, j-1) s1 (tail s2)
            DIAG  -> (++) `onBoth`
                   alignPair (show $ head s1) (show $ head s2) `tApp`
                   go (i-1, j-1) (tail s1) (tail s2)
        alignPair s1 s2 =
          let n = (max `on` length) s1 s2
          in (align '-' n) `onBoth` (s1, s2)
