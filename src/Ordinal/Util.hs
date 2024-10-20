{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ordinal.Util
  ( InfiniteList(..)
  , NonZeroNat(..)
  , iterateInfinite
  , nznList
  , nznOne
  , nznPlus
  , predecessor
  , successor
  )
where

import Numeric.Natural (Natural)
import Data.Types.Injective (Injective, to)
import Data.Types.Isomorphic (Iso)


-- | A variant of `Natural` without zero. `PlusOne 0` corresponds to 1,
-- `PlusOne 1`, to 2, etc. Conversely, applying `minusOne` to the value
-- representing n gives n-1
newtype NonZeroNat = PlusOne { minusOne :: Natural }
  deriving (Eq, Ord, Show)

instance Injective Natural (Maybe NonZeroNat) where
  to n = PlusOne <$> minusNaturalMaybe n 1
    where minusNaturalMaybe x y = if x < y then Nothing else Just $ x - y

instance Injective (Maybe NonZeroNat) Natural where
  to Nothing  = 0
  to (Just n) = minusOne n + 1

instance Iso Natural (Maybe NonZeroNat)

nznOne :: NonZeroNat
nznOne = PlusOne 0

nznPlus :: NonZeroNat -> NonZeroNat -> NonZeroNat
nznPlus (PlusOne n1) (PlusOne n2) = successor $ PlusOne $ n1 + n2

successor :: NonZeroNat -> NonZeroNat
successor n = PlusOne $ minusOne n + 1

predecessor :: NonZeroNat -> Maybe NonZeroNat
predecessor n = to $ minusOne n

data InfiniteList a = a :.: (InfiniteList a)

instance Functor InfiniteList where
  fmap f (x :.: xs) = f x :.: fmap f xs

iterateInfinite :: (a -> a) -> a -> InfiniteList a
iterateInfinite f x = x :.: fmap f (iterateInfinite f x)

nznList :: InfiniteList NonZeroNat
nznList = listFrom (PlusOne 0)
  where listFrom n = n :.: listFrom (successor n)
