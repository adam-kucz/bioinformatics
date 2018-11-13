{-# LANGUAGE ScopedTypeVariables #-}
module Util
  ( onBoth
  , bothApp
  , tApp
  , align
  , show2dArray
  , read2dArray
  ) where

import Control.Arrow (Arrow, (***), (&&&))
import Data.Array (Array, array, (!), bounds)
import Data.Ix (Ix, range)

-- import Debug.Trace (trace)

onBoth :: Arrow a => a b c -> a (b,b) (c,c)
onBoth f = f *** f

infixl 3 `onBoth`

bothApp :: Arrow a => (a b c, a b c') -> a b (c, c')
bothApp = uncurry (&&&)

infixl 3 `bothApp`
  
tApp :: Arrow a => (a b c, a b' c') -> a (b, b') (c, c')
tApp = uncurry (***)

infixl 3 `tApp`

align :: Char -> Int -> String -> String
align c n str = replicate (n - length str) c ++ str

show2dArray :: forall a i j. (Show i, Show j, Show a, Ix i, Ix j) =>
               Array (i, j) a -> String
show2dArray arr = unlines [unwords [arr' ! (i, j) | j <- js] | i <- is]
  where arr' = fmap show arr
        is = range $ fst `onBoth` bounds arr'
        js = range $ snd `onBoth` bounds arr'

read2dArray ::
  forall a i j. (Read i, Read j, Read a, Ix i, Ix j) =>
                String -> Array (i, j) a
read2dArray raw = array (minBound', maxBound') elems
  where (headerLine:dataLines) = lines raw
        colNames :: [j]
        colNames = fmap read $ words headerLine
        rowNames :: [i]
        rowNames = fmap (read . head . words) dataLines
        elems :: [((i, j), a)]
        elems = foldMap (processLine . words) dataLines
        processLine :: [String] -> [((i, j), a)]
        processLine (c:as) =
          fmap (\(a2, a) -> ((a1, a2), a)) .
          zip colNames $
          fmap read as
          where a1 = read c :: i
        processLine [] = error "Empty line in input for BLOSUM50"
        minBound' = (minimum rowNames, minimum colNames)
        maxBound' = (maximum rowNames, maximum colNames)
