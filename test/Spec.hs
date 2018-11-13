{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Function (on)
import qualified Data.List as L
import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
-- import Test.QuickCheck ((==>))

import Alignment (Dir(..), needlemanWunsch)
import BioTypes
import BioArbitrary ()
import Matrix (size, (!))

main :: IO ()
main = hspec $ nwSpec

basicScore :: forall a. Eq a => a -> a -> Int
basicScore x y = if x == y then 1 else -1

nwSpec :: Spec
nwSpec = 
  describe "NeedlemanWunsch" $ do
    prop "self-alignment is diagonal and scores length of self" $
         \(dnaSeq :: [DNABase]) ->
             let mat = needlemanWunsch dnaSeq dnaSeq 1 basicScore
             in let res = mat ! (size mat)
             in fst res == length dnaSeq && (snd res == DIAG || snd res == END)

    prop "general alignment never scores more than length of shorter" $
         \(dnaSeq1 :: [DNABase]) (dnaSeq2 :: [DNABase]) ->
             let mat = needlemanWunsch dnaSeq1 dnaSeq2 1 basicScore
             in let res = mat ! (size mat)
             in fst res <= min (length dnaSeq1) (length dnaSeq2)                

    prop "general alignment never scores less than negative length of longer" $
         \(dnaSeq1 :: [DNABase]) (dnaSeq2 :: [DNABase]) ->
             let mat = needlemanWunsch dnaSeq1 dnaSeq2 1 basicScore
             in let res = mat ! (size mat)
             in fst res >= - max (length dnaSeq1) (length dnaSeq2)

    modifyMaxSize (const 15) .
      prop "score is at most the length of longest common subsequence" $
        \(dnaSeq1 :: [DNABase]) (dnaSeq2 :: [DNABase]) ->
           let mat = needlemanWunsch dnaSeq1 dnaSeq2 1 basicScore
           in let res = mat ! (size mat)
           in fst res <= (maximum . fmap length $
                          (L.intersect `on` L.subsequences)
                            dnaSeq1 dnaSeq2)
