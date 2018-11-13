{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BWT
  ( transformBWT
  , inverseBWT
  , inverseBWTsteps
  , inverseBWT'
  , showBWT
  , showStringBWT
  ) where

import Control.Monad.Writer.Lazy (MonadWriter, tell)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.List (inits, sort, tails)

rotations :: forall a. [a] -> [[a]]
rotations xs = init $ zipWith (++) (tails xs) (inits xs)

transformBWT :: forall a. Ord a => [a] -> [Maybe a]
transformBWT = fmap last . sort . rotations . (Nothing:) . fmap Just

inverseBWT' :: forall a. Ord a => [a] -> [a]
inverseBWT' ls = head . (!! n) . iterate next $ replicate n []
  where n :: Int
        n = length ls
        next :: [[a]] -> [[a]]
        next = sort . zipWith (:) ls

inverseBWTsteps :: forall a m.
                   (MonadWriter [[[a]]] m, Ord a) => [a] -> m [a]
inverseBWTsteps ls = fmap head .
                     (!! n) .
                     iterate next .
                     return $
                     replicate n []
  where n :: Int
        n = length ls
        next :: m [[a]] -> m [[a]]
        next mxs = do
          xs <- mxs
          let xs' = zipWith (:) ls xs
          let xs'' = sort xs'
          tell [xs', xs'']          
          return xs''

inverseBWT :: forall a. Ord a => [a] -> [a]
inverseBWT ls = fmap fst . take (length ls) . iterate next $ minimum ls'
  where ls' :: [(a, Int)]
        ls' = zip ls [0..]
        pairs :: [((a, Int), (a, Int))]
        pairs = zip ls' (sort ls')
        next :: (a, Int) -> (a, Int)
        next x = snd . fromJust $ find ((==x) . fst) pairs

showBWT :: forall a. Show a => [Maybe a] -> String
showBWT = foldMap (maybe "$" show)

showStringBWT :: [Maybe Char] -> String
showStringBWT = foldMap (maybe "$" (:[]))

