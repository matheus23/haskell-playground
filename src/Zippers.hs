module Zippers where

data ListContext a
  = ListContext
  { leftOf :: [a]
  , rightOf :: [a]
  } deriving (Show, Eq, Read)

data Pair a
  = Pair a a

data PairContext a
  = PairContext
  { inLeft :: a
  , inRight :: a
  } deriving (Show, Eq, Read)

type Zipper context foc = (foc, context foc)

type PairOfListContext a = PairContext [a]

type PairOfListZipper a = Zipper PairContext a
