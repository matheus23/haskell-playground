{-# LANGUAGE ExistentialQuantification #-}
module ScrapYourTypeclasses where

data Functor f
  = forall a b . Functor
  { functorMap :: (a -> b) -> f a -> f b }
