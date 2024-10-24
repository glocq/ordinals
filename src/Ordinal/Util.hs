{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ordinal.Util
  ( InfiniteList(..)
  , flipOrder
  , iterateInfinite
  , mapFst
  , nats
  )
where

import Data.Strict.Sequence (Seq)
import Data.Strict.Tuple    (Pair(..))
import Numeric.Natural      (Natural)


flipOrder :: Ordering -> Ordering
flipOrder LT = GT
flipOrder EQ = EQ
flipOrder GT = LT

mapFst :: (a -> b) -> Seq (Pair a c) -> Seq (Pair b c)
mapFst f = fmap (\(x :!: y) -> f x :!: y)

infixr 6 :.:

data InfiniteList a = a :.: (InfiniteList a)

instance Functor InfiniteList where
  fmap f (x :.: xs) = f x :.: fmap f xs

-- | List of repeated applications of a given function on a given argument,
-- starting at 0 applications.
-- ```
-- iterateInfinite f x = x :.: f x :.: f (f x) :.: f (f (f x)) :.: ...
iterateInfinite :: (a -> a) -> a -> InfiniteList a
iterateInfinite f x = x :.: fmap f (iterateInfinite f x)

-- | All natural numbers, in order
nats :: InfiniteList Natural
nats = iterateInfinite (+ 1) 0
