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
import           System.Random                  (Random, RandomGen, random,
                                                 randomR)
import           Test.QuickCheck                (Arbitrary, Gen, arbitrary,
                                                 choose)
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

-- | This implementation of @show@ can be used to make your custom type
--   an instance of @Show@.
basicShow :: (Narrow a, Show (BaseType a)) =>  a -> String
basicShow = show . wide

-- | This implementation of @readPrec@ can be used to make your custom
--   type an instance of @Read@.
basicReadPrec
  :: (Narrow a, Ord a, Bounded a, Read (BaseType a))
  => TR.ReadPrec a
basicReadPrec = fmap (narrow) TR.readPrec

-- | This implementation of @randomR@ can be used to make your custom
--   type an instance of @Random@.
basicRandomR
  :: (Narrow a, Ord a, Bounded a, Random (BaseType a), RandomGen g)
  => (a, a) -> g -> (a, g)
basicRandomR (a, b) g = (narrow x, g')
  where (x, g') = randomR (wide a, wide b) g

-- -- | This implementation of @random@ can be used to make your custom
-- --   type an instance of @Random@.
-- basicRandom
--   :: (Narrow a, Ord a, Bounded a, Random (BaseType a), RandomGen g)
--   => g -> (a, g)
-- basicRandom = basicRandomR (minBound, maxBound)

-- | This implementation of @arbitrary@ can be used to make your custom
--   type an instance of @Arbitrary@.
basicArbitrary
  :: forall a . (Narrow a, Ord a, Bounded a, Random (BaseType a))
  => Gen a
basicArbitrary = narrow <$> choose (a, b)
    where a = wide (minBound :: a)
          b = wide (maxBound :: a)

-- | This implementation of @express@ can be used to make your custom
--   type an instance of @Diploid@.
basicExpress
  :: (Narrow a, Ord a, Bounded a, Num (BaseType a), Fractional (BaseType a))
  => a -> a -> a
basicExpress x y = narrow $ (wide x + wide y)/2

-- | This implementation of @+@ can be used to make your custom
--   type an instance of @Num@.
basicAdd :: (Narrow a, Ord a, Bounded a, Num (BaseType a)) => a -> a -> a
basicAdd x y = narrow (wide x + wide y)

-- | This implementation of @-@ can be used to make your custom
--   type an instance of @Num@.
basicSubtract
  :: (Narrow a, Ord a, Bounded a, Num (BaseType a)) => a -> a -> a
basicSubtract x y = narrow (wide x - wide y)

-- | This implementation of @*@ can be used to make your custom
--   type an instance of @Num@.
basicMultiply
  :: (Narrow a, Ord a, Bounded a, Num (BaseType a)) => a -> a -> a
basicMultiply x y = narrow (wide x * wide y)

-- | This implementation of @abs@ can be used to make your custom
--   type an instance of @Num@.
basicAbs
  :: (Narrow a, Ord a, Bounded a, Num (BaseType a)) => a -> a
basicAbs = narrow . abs . wide

-- | This implementation of @signum@ can be used to make your custom
--   type an instance of @Num@.
basicSignum
  :: (Narrow a, Ord a, Bounded a, Num (BaseType a)) => a -> a
basicSignum = narrow . signum . wide

-- | This implementation of @abs@ can be used to make your custom
--   type an instance of @Num@.
basicFromInteger
  :: (Narrow a, Ord a, Bounded a, Num (BaseType a)) => Integer -> a
basicFromInteger = narrow . fromInteger

-- | This implementation of @negate@ can be used to make your custom
--   type an instance of @Num@.
basicNegate
  :: (Narrow a, Ord a, Bounded a, Num (BaseType a)) => a -> a
basicNegate = narrow . negate . wide

-- | This implementation of @/@ can be used to make your custom
--   type an instance of @Fractional@.
basicDivide
  :: (Narrow a, Ord a, Bounded a, Fractional (BaseType a)) => a -> a -> a
basicDivide x y = narrow (wide x / wide y)

-- | This implementation of @fromRational@ can be used to make your custom
--   type an instance of @Fractional@.
basicFromRational
  :: (Narrow a, Ord a, Bounded a, Fractional (BaseType a)) => Rational -> a
basicFromRational = narrow . fromRational

-- | This implementation of @pi@ can be used to make your custom
--   type an instance of @Floating@.
basicPi :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a
basicPi = narrow pi

-- | This implementation of @exp@ can be used to make your custom
--   type an instance of @Floating@.
basicExp
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicExp = narrow . exp . wide

-- | This implementation of @log@ can be used to make your custom
--   type an instance of @Floating@.
basicLog
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicLog = narrow . log . wide

-- | This implementation of @sin@ can be used to make your custom
--   type an instance of @Floating@.
basicSin
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicSin = narrow . sin . wide

-- | This implementation of @cos@ can be used to make your custom
--   type an instance of @Floating@.
basicCos
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicCos = narrow . cos . wide

-- | This implementation of @asin@ can be used to make your custom
--   type an instance of @Floating@.
basicAsin
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicAsin = narrow . asin . wide

-- | This implementation of @acos@ can be used to make your custom
--   type an instance of @Floating@.
basicAcos
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicAcos = narrow . acos . wide

-- | This implementation of @atan@ can be used to make your custom
--   type an instance of @Floating@.
basicAtan
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicAtan = narrow . atan . wide

-- | This implementation of @sinh@ can be used to make your custom
--   type an instance of @Floating@.
basicSinh
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicSinh = narrow . sinh . wide

-- | This implementation of @cosh@ can be used to make your custom
--   type an instance of @Floating@.
basicCosh
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicCosh = narrow . cosh . wide

-- | This implementation of @asinh@ can be used to make your custom
--   type an instance of @Floating@.
basicAsinh
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicAsinh = narrow . asinh . wide

-- | This implementation of @acosh@ can be used to make your custom
--   type an instance of @Floating@.
basicAcosh
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicAcosh = narrow . acosh . wide

-- | This implementation of @atanh@ can be used to make your custom
--   type an instance of @Floating@.
basicAtanh
  :: (Narrow a, Ord a, Bounded a, Floating (BaseType a)) => a -> a
basicAtanh = narrow . atanh . wide

-- | This implementation of @toRational@ can be used to make your custom
--   type an instance of @Real@.
basicToRational
  :: (Narrow a, Ord a, Bounded a, Real (BaseType a)) => a -> Rational
basicToRational = toRational . wide


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
  show = basicShow . unpack

instance (Narrow a, Ord a, Bounded a, Read (BaseType a)) => Read (UseNarrow a) where
  readPrec = fmap UN basicReadPrec

instance (Narrow a, Ord a, Bounded a, Random (BaseType a))
  => Random (UseNarrow a) where
  randomR (a, b) g = (UN x, g')
    where (x, g') = basicRandomR (unpack a, unpack b) g
  random = randomR (UN minBound, UN maxBound)

instance (Narrow a, Ord a, Bounded a, Random (BaseType a))
  => Arbitrary (UseNarrow a) where
  arbitrary = fmap UN basicArbitrary

instance (Narrow a, Ord a, Bounded a, Num (BaseType a),
          Fractional (BaseType a))
  => Diploid (UseNarrow a) where
  express x y = UN $ basicExpress (unpack x) (unpack y)

instance (Narrow a, Ord a, Bounded a, Num (BaseType a)) => Num (UseNarrow a) where
  x + y = UN $ basicAdd (unpack x) (unpack y)
  x - y = UN $ basicSubtract (unpack x) (unpack y)
  x * y = UN $ basicMultiply (unpack x) (unpack y)
  abs = UN . basicAbs . unpack
  signum = UN . basicSignum . unpack
  fromInteger = UN . basicFromInteger
  negate = UN . basicNegate . unpack

instance (Narrow a, Ord a, Bounded a, Fractional (BaseType a))
  => Fractional (UseNarrow a) where
  x / y = UN $ basicDivide (unpack x) (unpack y)
  fromRational = UN . basicFromRational

instance (Narrow a, Ord a, Bounded a, Fractional (BaseType a), Floating (BaseType a))
  => Floating (UseNarrow a) where
  pi = UN basicPi
  exp = UN . basicExp . unpack
  log = UN . basicLog . unpack
  sin = UN . basicSin . unpack
  cos = UN . basicCos . unpack
  asin = UN . basicAsin . unpack
  acos = UN . basicAcos . unpack
  atan = UN . basicAtan . unpack
  sinh = UN . basicSinh . unpack
  cosh = UN . basicCosh . unpack
  asinh = UN . basicAsinh . unpack
  acosh = UN . basicAcosh . unpack
  atanh = UN . basicAtanh . unpack

instance (Narrow a, Ord a, Bounded a, Fractional (BaseType a), Real (BaseType a))
  => Real (UseNarrow a) where
  toRational = basicToRational . unpack
