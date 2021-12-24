------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.Narrow
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Numeric values constrained to lie in an interval [a, b].
-- Note: The endpoints are included in the interval.
--
------------------------------------------------------------------------
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

module ALife.Creatur.Gene.Numeric.Narrow
  (
    Narrow,
    BaseType,
    unsafeConstructor,
    narrow,
    wide,
    crop,
    apply,
    UseNarrow(..)
  ) where

import           ALife.Creatur.Genetics.Diploid (Diploid, express)
import           System.Random                  (Random, random, randomR)
import           Test.QuickCheck                (Arbitrary, arbitrary, choose)
import qualified Text.Read                      as TR

-- | Values that are constrained to lie in an interval.
class Narrow a where
  type BaseType a

  -- | Unsafe constructor for the instance type.
  --   Instance classes should not export this function.
  unsafeConstructor :: BaseType a -> a

  -- | Constructor for the instance type.
  --   If the argument is outside the allowed interval, an error is
  --   thrown.
  narrow :: (Ord a, Bounded a) => BaseType a -> a
  narrow x | x' < (minBound :: a) = error "too small"
           | x' > (maxBound :: a) = error "too big"
           | otherwise     = x'
    where x' = unsafeConstructor x :: a

  -- | Deconstructor for the instance type
  wide :: a -> BaseType a

  -- | Constructor for the instance type.
  --   If the argument is outside the allowed bounds, an error is
  --   thrown.
  crop :: (Ord a, Bounded a) => BaseType a -> a
  crop = max minBound . min maxBound . unsafeConstructor

  -- | Applies the function to the value.
  --   If the result is outside the allowed interval, an error is
  --   thrown.
  apply :: (Ord a, Bounded a) => (BaseType a -> BaseType a) -> a -> a
  apply f = narrow . f . wide

-- | This type is just used as a template for deriving instances.
--
--   Example:
--       deriving via (UseNarrow MyType) instance Show MyType
newtype UseNarrow a = UN a
  deriving (Eq, Ord)

-- | Internal method
unpack :: UseNarrow a -> a
unpack (UN x) = x

instance (Narrow a, Show (BaseType a)) => Show (UseNarrow a) where
  show = show . wide . unpack

instance (Narrow a, Ord a, Bounded a, Read (BaseType a)) => Read (UseNarrow a) where
  readPrec = fmap (UN . narrow) TR.readPrec

instance (Narrow a, Ord a, Bounded a, Random (BaseType a))
  => Random (UseNarrow a) where
  randomR (a, b) g = (UN $ narrow x, g')
    where (x, g') = randomR (wide $ unpack a, wide $ unpack b) g
  random = randomR (UN minBound, UN maxBound)

instance (Narrow a, Ord a, Bounded a, Random (BaseType a))
  => Arbitrary (UseNarrow a) where
  arbitrary = UN . narrow <$> choose (a, b)
    where a = wide (minBound :: a)
          b = wide (maxBound :: a)

instance (Narrow a, Ord a, Bounded a, Num (BaseType a),
          Fractional (BaseType a))
  => Diploid (UseNarrow a) where
  express x y = UN . narrow $ ((wide $ unpack x) + (wide $ unpack y))/2

instance (Narrow a, Ord a, Bounded a, Num (BaseType a)) => Num (UseNarrow a) where
  x + y = UN . narrow $ (wide $ unpack x) + (wide $ unpack y)
  x - y = UN . narrow $ (wide $ unpack x) - (wide $ unpack y)
  x * y = UN . narrow $ (wide $ unpack x) * (wide $ unpack y)
  abs = UN . narrow . abs . wide . unpack
  signum = UN . narrow . signum . wide . unpack
  fromInteger = UN . narrow . fromInteger
  negate = UN . narrow . negate . wide . unpack

instance (Narrow a, Ord a, Bounded a, Fractional (BaseType a))
  => Fractional (UseNarrow a) where
  x / y = UN . narrow $ (wide $ unpack x) / (wide $ unpack y)
  fromRational = UN . narrow . fromRational

instance (Narrow a, Ord a, Bounded a, Fractional (BaseType a), Floating (BaseType a))
  => Floating (UseNarrow a) where
  pi = UN $ narrow pi
  exp = UN . narrow . exp . wide . unpack
  log = UN . narrow . log . wide . unpack
  sin = UN . narrow . sin . wide . unpack
  cos = UN . narrow . cos . wide . unpack
  asin = UN . narrow . asin . wide . unpack
  acos = UN . narrow . acos . wide . unpack
  atan = UN . narrow . atan . wide . unpack
  sinh = UN . narrow . sinh . wide . unpack
  cosh = UN . narrow . cosh . wide . unpack
  asinh = UN . narrow . asinh . wide . unpack
  acosh = UN . narrow . acosh . wide . unpack
  atanh = UN . narrow . atanh . wide . unpack

instance (Narrow a, Ord a, Bounded a, Fractional (BaseType a), Real (BaseType a))
  => Real (UseNarrow a) where
  toRational = toRational . wide . unpack
