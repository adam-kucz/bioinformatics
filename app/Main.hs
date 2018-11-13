{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when, MonadPlus, mzero)
import Control.Monad.Writer (runWriter)
import Data.Array (Ix, Array, (!))
import Data.Foldable (traverse_)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Alignment (needlemanWunsch, showAlignment, showAlignedSequences)
import BioTypes (AminoAcid, DNABase, RNABase)
import BWT (inverseBWTsteps, inverseBWT, transformBWT, showStringBWT)
import Clustering (sqDist, kMeans, kCenter)
import Matrix (fromList, size, Matrix)
import MatrixInteraction (readMatrix, showMatrix)
import Phylogenetics (TreeLike, neighborJoining, drawTree, Distance, upgma)
import Util (read2dArray)

data Action = Alignment | Phylogenetics | BWtransform | Clustering | Exit
  deriving (Read, Eq)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  horizontalLine
  showMenu
  c <- getChoice
  case c of
    Alignment     -> alignment >> main
    Phylogenetics -> phylogenetics >> main
    BWtransform   -> bwTransform >> main
    Clustering    -> clustering >> main
    Exit          -> return ()
  where showMenu :: IO ()
        showMenu = putStr $ "Bioinformatics algorithms\n" ++
                            "1) Alignment     - Needleman-Wunsch\n" ++
                            "2) Phylogenetics - UPGMA and neighbor joining\n" ++
                            "3) BWtransform   - Burrows-Wheeler transform\n" ++
                            "4) Clustering    - k-means and k-center " ++
                                              "clustering\n" ++
                            "0) Exit          - exits the application\n"
        getChoice :: IO Action
        getChoice = do
           n <- safeGet "Your choice: "
           return $ case n of
             1 -> Alignment
             2 -> Phylogenetics
             3 -> BWtransform
             4 -> Clustering
             _ -> Exit

safeRead :: (MonadPlus m, Read a) => String -> m a
safeRead = return . read

safeGet :: Read a => String -> IO a
safeGet msg = putStr msg >> getLine >>= safeRead

stripComments :: String -> String
stripComments = unlines . filter ((/='#') . head) . lines

horizontalLine :: IO ()
horizontalLine = putStrLn $ replicate 50 '-'

class (Ix a, Show a, Read a) => BioIndex a where
instance BioIndex DNABase where
instance BioIndex RNABase where
instance BioIndex AminoAcid where

data Alphabet = DNA | RNA | AAs
  deriving (Read, Eq)

alignment :: IO ()
alignment = do
  alphabet <- getAlphabet
  case alphabet of
    DNA -> alignment' (phantom :: DNABase)
    RNA -> alignment' (phantom :: RNABase)
    AAs -> alignment' (phantom :: AminoAcid)
  where phantom :: forall i. BioIndex i => i
        phantom = undefined
        getAlphabet :: IO Alphabet
        getAlphabet = safeGet "Type of sequence (DNA, RNA or AAs): "

alignment' :: forall i. BioIndex i => i -> IO ()
alignment' _ = do
  scoringMatrix <- getMatrix "File with scoring matrix: "
  seq1          <- getSeq "First sequence to align: "
  seq2          <- getSeq "Second sequence to align: "
  d             <- safeGet "Gap penalty: "
  let score = curry (scoringMatrix !)
  let aligned = needlemanWunsch seq1 seq2 d score
  let (s1, s2) = showAlignedSequences seq1 seq2 aligned
  putStrLn "\nAligned seqences"
  putStrLn s1
  putStrLn s2
  putStrLn "\nAlignment matrix"
  sequence_ . fmap putStrLn . showAlignment seq1 seq2 $ aligned
  putStrLn ""
  where getSeq :: String -> IO [i]
        getSeq msg = putStr msg >> fmap readSeq getLine
        readSeq :: String -> [i]
        readSeq "" = []
        readSeq s  = case reads s of
          [(i, s')] -> i:readSeq s'
          _         -> error $ "Cannot read from '" ++ s ++ "''"
        getMatrix :: (Read n, Num n) => String -> IO (Array (i, i) n)
        getMatrix msg = do
          putStr msg
          file <- getLine
          read2dArray . stripComments <$> readFile file

phylogenetics :: IO ()
phylogenetics = do
  horizontalLine
  putStrLn $ "Which algorithm do you want to run?\n" ++
             "1) UPGMA\n" ++ 
             "2) Neighbor joining\n" ++
             "3) Both\n" ++
             "0) Neither (go back)\n"
  choice  <- safeGet "Choice: "
  if choice == 0
    then return ()
    else do
      distMat <- getMatrix "File with distance matrix: "
      putStrLn "\nDistance matrix:"
      putStrLn $ showMatrix distMat
      let dist = curry (distMat !)
      let ls = [1..fst $ size distMat]
      when (mod choice 2 == 1) $ printAlgorithm upgma "UPGMA tree:" ls dist
      when (choice >= 2) $ printAlgorithm neighborJoining
                                          "Neighbor joining tree:" ls dist
      phylogenetics
  where getMatrix :: String -> IO (Matrix Int Distance)
        getMatrix msg = do
          putStr msg
          file <- getLine
          rows <- stripComments <$> readFile file
          maybe mzero return $ readMatrix rows
        printAlgorithm :: (TreeLike t e, Show e) =>
                          ([Int] -> (Int -> Int -> Distance) -> Maybe t) ->
                          String -> [Int] -> (Int -> Int -> Distance) -> IO ()
        printAlgorithm algo msg ls dist = do
          let Just tree = algo ls dist
          putStrLn msg
          putStrLn $ drawTree tree
          putStrLn "" 

bwTransform :: IO ()
bwTransform = do
  horizontalLine
  putStrLn $ "Do you want to run Burrows-Wheeler transform or its inverse?\n" ++
             "1) Burrows-Wheeler transform\n" ++ 
             "2) Inverse\n" ++
             "3) Inefficient inverse with all steps shown\n" ++
             "0) None (go back)\n"
  choice    <- safeGet "Choice: "
  if choice == 0
     then return ()
     else do
       sequence' <- putStr "Sequence: " >> getLine
       case choice of
         1 -> putStrLn . showStringBWT $ transformBWT sequence'
         2 -> putStrLn $ inverseBWT sequence'
         3 -> do let (res, steps) = runWriter $ inverseBWTsteps sequence'
                 putStrLn $ "Inverted: " ++ res
                 mat <- maybe mzero return $ fromList steps
                 putStrLn $ showMatrix mat
         _ -> return ()
       bwTransform

clustering :: IO ()
clustering = do
  horizontalLine
  putStrLn $ "Which algorithm do you want to run?\n" ++
             "1) k-means\n" ++ 
             "2) k-center\n" ++
             "3) Both\n" ++
             "0) Neither (go back)\n"
  choice  <- safeGet "Choice: "
  if choice == 0
    then return ()
    else do
      k       <- safeGet "Number of clusters: "
      points  <- getPoints "File with points (one per line, format [a,b,..,q]): "
      putStrLn $ "\nRead " ++ show (length points) ++ " points"
      when (mod choice 2 == 1) $ printAlgorithm kMeans "k-means:" k points
      when (choice >= 2) $ printAlgorithm (kCenter sqDist) "k-center" k points
  when (choice /= 0) clustering
  where getPoints :: String -> IO [[Float]]
        getPoints msg = do
          putStr msg
          file <- getLine
          rows <- lines . stripComments <$> readFile file
          return $ read <$> rows
        printAlgorithm :: (Int -> [[Float]] -> IO [[Float]]) ->
                          String -> Int -> [[Float]] -> IO ()
        printAlgorithm algo msg ls dist = do
          putStrLn msg
          centers <- algo ls dist
          putStrLn "Centers:"
          traverse_ print centers
          putStrLn "" 
