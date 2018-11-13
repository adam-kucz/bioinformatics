{-# LANGUAGE ScopedTypeVariables #-}
module MatrixInteraction
  ( stringifyLabeledMatrix
  , stringifyMatrix
  , showLabeledMatrix
  , showMatrix
  , interpretMatrix
  , readMatrix
  ) where

import Data.Ix (Ix)
import Data.List (transpose, intercalate)
import qualified Data.Text as T

import Matrix (Matrix, toList, fromList)
import Util (align)

stringifyLabeledMatrix ::
  forall a b i. (Show a, Show b, Ix i, Num i) =>
  String -> Char -> [a] -> [a] -> Matrix i b -> [String]
stringifyLabeledMatrix delim empty rowLabels colLabels mat =
  fmap joinCells contents
  where contents :: [[String]]
        contents = colNames : rows
          where rows = fmap (uncurry (:)) .
                       zip ("":fmap show rowLabels) .
                       toList $ show <$> mat
        colNames :: [String]
        colNames = "":"":fmap show colLabels
        joinCells :: [String] -> String
        joinCells cells = intercalate delim .
                          fmap (uncurry $ align empty) $
                          zip colWidths cells
        colWidths :: [Int]
        colWidths = fmap (maximum . fmap length) $ transpose contents

stringifyMatrix ::
  forall a i. (Show a, Ix i, Num i) =>
  String -> Char -> Matrix i a -> [String]
stringifyMatrix delim empty mat = fmap joinCells contents
  where contents = toList $ show <$> mat
        joinCells cells = intercalate delim .
                          fmap (uncurry $ align empty) $
                          zip colWidths cells
        colWidths = fmap (maximum . fmap length) $ transpose contents

showLines :: [String] -> String
showLines = intercalate "\n"

showLabeledMatrix ::
  forall a b i. (Show a, Show b, Ix i, Num i) =>
  [a] -> [a] -> Matrix i b -> String
showLabeledMatrix s1 s2 = showLines .
                          stringifyLabeledMatrix "|" ' ' s1 s2

showMatrix :: forall a i. (Show a, Ix i, Num i) => Matrix i a -> String
showMatrix = showLines . stringifyMatrix "|" ' '

interpretMatrix :: forall a i. (Ix i, Enum i, Num i, Read a) =>
                   String -> String -> String -> Maybe (Matrix i a)
interpretMatrix lineSep elemSep input = fromList elems
  where inRows = T.splitOn (T.pack lineSep) $ T.pack input
        inElems = T.splitOn (T.pack elemSep) <$> inRows
        elems = fmap (read . T.unpack) <$> inElems

readMatrix :: forall a. (Show a, Read a) => String -> Maybe (Matrix Int a)
readMatrix input = fromList $ fmap read <$> words <$> lines input
