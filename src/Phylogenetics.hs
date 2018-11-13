{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Phylogenetics
  ( upgma
  , drawTree
  , neighborJoining
  , Distance
  , TreeLike
  ) where

import Control.Arrow (first)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List ((\\))
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tree (Tree(..))
import qualified Data.Tree as T

data BTree b a = Leaf a | Branch b (BTree b a) (BTree b a)
   deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Distance = Float
type UpgmaTree s = BTree Distance s

type DistanceMatrix a = a -> a -> Distance
type UpgmaTreeDist s = DistanceMatrix (UpgmaTree s)

class TreeLike t e | t -> e where
  toTree :: t -> Tree e

instance TreeLike (BTree b a) (Either b a) where
  toTree (Leaf a)         = Node (Right a) []
  toTree (Branch b t1 t2) = Node (Left b) [toTree t1, toTree t2]

drawTree :: forall t a. (Show a, TreeLike t a) => t -> String 
drawTree = T.drawTree . fmap show . toTree

upgma :: forall s. Eq s => [s] -> DistanceMatrix s -> Maybe (UpgmaTree s)
upgma species mat = go initialDist $ fmap Leaf species
  where go :: UpgmaTreeDist s -> [UpgmaTree s] -> Maybe (UpgmaTree s)
        go _ [] = Nothing
        go _ [cluster] = Just cluster
        go dist cs =
          let (c1,c2) = minimumBy (compare `on` uncurry dist) $
                        clusterCombs cs
          in let cnew = merge dist c1 c2
          in go (updateDist c1 c2 cnew dist) $ cnew : (cs \\ [c1,c2])
        clusterCombs :: [UpgmaTree s] -> [(UpgmaTree s, UpgmaTree s)]
        clusterCombs [] = []
        clusterCombs (c:cs) = fmap (c,) cs ++ clusterCombs cs
        initialDist (Leaf s1) (Leaf s2) = mat s1 s2
        initialDist _ _ = 1/0 -- infinity
        merge dist c1 c2 = Branch (dist c1 c2 / 2) c1 c2

updateDist ::
  forall s. Eq s => UpgmaTree s -> UpgmaTree s -> UpgmaTree s ->
                    UpgmaTreeDist s -> UpgmaTreeDist s
updateDist c1 c2 cnew dist c1' c2' = case (c1' == cnew, c2' == cnew) of
  (True, True) -> 0
  (True,    _) -> (n1 * dist c1 c2' + n2 * dist c2 c2') / (n1 + n2)
  (   _, True) -> (n1 * dist c1' c1 + n2 * dist c1' c2) / (n1 + n2)
  (   _,    _) -> dist c1' c2'
  where n1 = fromIntegral $ length c1
        n2 = fromIntegral $ length c2

data ETree e a = Terminal a | Internal [(e, ETree e a)]
   deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance TreeLike (ETree e a) (Maybe e, Maybe a) where
  toTree = go Nothing
    where go e (Terminal a)  = Node (e, Just a) []
          go e (Internal ls) = Node (e, Nothing) $
                               uncurry go . first Just <$> ls

type NeighborJoinTree s = ETree Distance s

neighborJoining ::
  forall s. (Ord s, Show s) => [s] -> DistanceMatrix s ->
            Maybe (NeighborJoinTree s)
neighborJoining species mat = go mat' .
                              S.map Terminal $
                              S.fromList species
  where go :: DistanceMatrix (NeighborJoinTree s) ->
              Set (NeighborJoinTree s) -> Maybe (NeighborJoinTree s)
        go dist els = case S.toList els of
          []                    -> Nothing
          [t]                   -> Just t
          [t@(Internal ls), t'] -> Just . Internal $ (dist t t', t'):ls
          [t, t'@(Internal ls)] -> Just . Internal $ (dist t t', t ):ls
          _                     -> go dist' els'
          where n = fromIntegral $ S.size els
                dstar k l = (n - 2) * dist k l - totDist k - totDist l
                totDist k = sum .
                            fmap (dist k) .
                            filter (/=k) $
                            S.toList els
                slist = filter (uncurry (/=)) $
                        (,) <$> S.toList els <*> S.toList els
                (i, j) = minimumBy (comparing $ uncurry dstar) slist
                deltaij = (totDist i - totDist j) / (n - 2)
                limbleni = (dist i j + deltaij) / 2
                limblenj = limbleni - deltaij
                subtree = Internal [(limbleni, i), (limblenj, j)]
                dist' k l
                  | k == subtree = (dist i l + dist j l - dist i j) / 2
                  | l == subtree = (dist i k + dist j k - dist i j) / 2
                  | otherwise    = dist k l
                els' = S.insert subtree . S.delete i $ S.delete j els
        mat' (Terminal s1) (Terminal s2) = mat s1 s2
        mat' t1 t2 = error $
          "initial matrix only has entries for terminal nodes, " ++
          "requested " ++ show t1 ++ ", " ++ show t2
          
