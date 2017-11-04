{-# LANGUAGE MonadComprehensions #-}
module Infinity where

import Control.Monad
import Data.List

newtype Infinite a = Infinite { toList :: [a]}

instance Functor Infinite where
  fmap f = Infinite . map f . toList

instance Applicative Infinite where
  (<*>) = ap
  pure = return

instance Monad Infinite where
  return = Infinite . return
  infiniteList >>= f = combineInfinite (f <$> infiniteList)

combineInfinite :: Infinite (Infinite a) -> Infinite a
combineInfinite (Infinite infinites) =
  Infinite (combineInfiniteLists 0 (map toList infinites))

combineInfiniteLists :: Integer -> [[a]] -> [a]
combineInfiniteLists n infiniteLists =
  heads ++ combineInfiniteLists (n+1) rest
  where
    (heads, rest) = extractNHeads n infiniteLists

extractNHeads :: Integer -> [[a]] -> ([a], [[a]])
extractNHeads n ls =
  (heads, tails ++ unchanged)
  where
    (toRemoveHeadFrom, unchanged) = genericSplitAt n ls
    (headsSingletons, tails) = unzip (map (splitAt 1) toRemoveHeadFrom)
    heads = concat headsSingletons

triples :: [(Integer, Integer, Integer)]
triples = toList [(x, y, z) | x <- Infinite [1..], y <- Infinite [1..], z <- Infinite [1..]]

hamming :: [Integer]
hamming = toList [2^a * 3^b * 5^c | a <- Infinite [0..], b <- Infinite [0..], c <- Infinite [0..]]
