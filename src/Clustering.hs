{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Clustering
  ( kCenter
  , kMeans
  , FracVec(..)
  ) where

import Control.Monad (MonadPlus, mzero)
import Control.Monad.Random.Class (MonadRandom, getRandomR)
import Data.Foldable (toList, maximumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (transpose, partition)

randomSublist :: forall a m. (MonadRandom m, MonadPlus m) => Int -> [a] -> m [a]
randomSublist 0 _  = return []
randomSublist _ [] = mzero
randomSublist n xs = do
  r <- getRandomR (0, length xs - 1)
  let (a, x:as) = splitAt r xs
  (x:) <$> randomSublist (n-1) (a ++ as)

randomChoice :: forall a m. (MonadRandom m, MonadPlus m) => [a] -> m a
randomChoice [] = mzero
randomChoice xs = (xs !!) <$> getRandomR (0, length xs - 1)

kCenter :: forall a d m. (MonadRandom m, MonadPlus m, Ord d) =>
           (a -> a -> d) -> Int -> [a] -> m [a]
kCenter dist k xs
  | k-1 >= 0  = nMoreCenters (k-1) . (:[]) <$> randomChoice xs
  | otherwise = mzero
  where nMoreCenters :: Int -> [a] -> [a]
        nMoreCenters 0 cs = cs
        nMoreCenters n cs = nMoreCenters (n-1) .
                            (:cs) $
                            maximumBy (comparing $ distToCenters cs) xs
        distToCenters :: [a] -> a -> d
        distToCenters cs x = minimum $ dist x <$> cs

class FracVec d a | a -> d where
  sqDist :: a -> a -> d
  sqDistCenter :: forall f. Foldable f => f a -> a

  default sqDist :: (a ~ d, Fractional a) => a -> a -> d
  sqDist = fractSqDist

  default sqDistCenter :: forall f. (Fractional a, Foldable f) => f a -> a
  sqDistCenter = fractSqDistCenter

fractSqDist :: forall a. Fractional a => a -> a -> a
fractSqDist a b = (a-b)^2

fractSqDistCenter :: forall f a. (Fractional a, Foldable f) => f a -> a
fractSqDistCenter xs = sum xs / fromIntegral (length xs)

instance FracVec Float Float where
instance FracVec Double Double where

instance (Num d, FracVec d a) => FracVec d [a] where
  sqDist a b = sum $ zipWith sqDist a b
  sqDistCenter xs = fmap sqDistCenter . transpose $ toList xs

kMeans ::
  forall a d m. (FracVec d a, MonadRandom m, MonadPlus m, Ord d) =>
  Int -> [a] -> m [a]
kMeans k xs =
  fmap (fmap (sqDistCenter . (fmap fst))) .
  fmap fixFrom .
  fmap (assign xs') $
  randomSublist k xs
  where xs' :: [(a, Int)]
        xs' = zip xs [0..]
        fixFrom :: [[(a, Int)]] -> [[(a, Int)]]
        fixFrom cs
          | ((==) `on` fmap (fmap snd)) cs cs' = cs'
          | otherwise                          = fixFrom cs'
          where cs' = assign xs' $ sqDistCenter . (fmap fst) <$> cs
        assign ::  [(a, Int)] -> [a] -> [[(a, Int)]]
        assign _  []     = error "Clustering.assign: no centers"
        assign ls [_]    = [ls]
        assign ls (c:cs) = rs : assign rs' cs
          where (rs, rs')        = partition closestToC ls
                closestToC (x,_) = sqDist x c < distToCenters cs x
        distToCenters :: [a] -> a -> d
        distToCenters cs x = minimum $ sqDist x <$> cs
