{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Matrix
  ( Matrix
  , matrix
  , fromAssocList
  , fromList
  , toList
  , (!)
  , size
  , mmap
  ) where

import Data.Array (Array, array, (!), bounds, listArray, assocs)
import Data.Ix (Ix, range)

type Matrix i e = Array (i, i) e

size :: forall i e. Matrix i e -> (i, i)
size = snd . bounds

matrix :: forall i e. (Ix i, Num i) =>
          (i, i) -> ((i, i) -> e) -> Matrix i e
matrix (i, j) f = listArray ixBounds $ f <$> range ixBounds
  where ixBounds = ((1, 1), (i, j))

fromAssocList :: forall i e. (Ix i, Num i) =>
                 (i, i) -> [((i, i), e)] -> Matrix i e
fromAssocList = array . ((1, 1),)

mmap :: forall i a b. (Ix i, Num i) =>
        ((i, i) -> a -> b) -> Matrix i a -> Matrix i b
mmap f m = listArray (bounds m) $ uncurry f <$> assocs m

fromList :: forall i a. (Ix i, Enum i, Num i) =>
            [[a]] -> Maybe (Matrix i a)
fromList ls = if valid then Just $ fromAssocList inds als else Nothing
  where als :: [((i, i), a)]
        als = foldMap (uncurry $ \i -> zip $ (i,) <$> [1..]) $
              zip [1..] ls
        inds :: (i, i)
        inds = (f ls, f $ head ls)
          where f = fromInteger . toInteger . length
        valid = all (==n) $ length <$> ls
          where n = length $ head ls

toList :: forall i a. (Ix i, Num i) => Matrix i a -> [[a]]
toList m = [[m ! (i, j) | j <- range (1, jmax)] | i <- range (1, imax)]
  where (imax, jmax) = snd $ bounds m
