------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.UnitInterval
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Numbers on the unit interval (0 to 1, inclusive).
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ALife.Creatur.Gene.Numeric.UnitInterval
  (
    UIDouble,
    narrow,
    wide,
    diff,
    makeSimilar
  ) where

import           ALife.Creatur.Gene.Numeric.Narrow (BaseType, Narrow,
                                                    UseNarrow (..), narrow,
                                                    unsafeConstructor, wide)
import qualified ALife.Creatur.Genetics.BRGCWord8  as W8
import           ALife.Creatur.Genetics.Diploid    (Diploid)
import           Control.DeepSeq                   (NFData)
import qualified Data.Datamining.Pattern.Numeric   as N
import           Data.Serialize                    (Serialize)
import           GHC.Generics                      (Generic)
import           System.Random                     (Random)
import           Test.QuickCheck                   (Arbitrary)

-- | A number on the unit interval
newtype UIDouble = UIDouble Double
  deriving (Eq, Ord, Generic)
  deriving anyclass (W8.Genetic)
  deriving newtype (NFData, Serialize)
  deriving (Show, Read, Random, Arbitrary, Diploid, Num,
            Fractional, Floating, Real)
    via (UseNarrow UIDouble)

instance Bounded UIDouble where
  minBound = UIDouble 0
  maxBound = UIDouble 1

instance Narrow UIDouble where
  type BaseType UIDouble = Double
  unsafeConstructor = UIDouble
  wide (UIDouble x) = x

-- | Internal method.
typeWidth :: (Narrow a, Bounded a, BaseType a ~ Double) => a -> Double
typeWidth x = wide (maxBound `asTypeOf` x) - wide (minBound `asTypeOf` x)

diff :: (Narrow a, Bounded a, BaseType a ~ Double) => a -> a -> UIDouble
diff x y = narrow (difference / scale) :: UIDouble
  where difference = abs $ (wide x) - (wide y) :: Double
        scale = typeWidth x

-- | Convenience wrapper for makeSimilar on narrow types.
makeSimilar
  :: (Narrow a, Bounded a, Ord a, BaseType a ~ Double)
  => a -> UIDouble -> a -> a
makeSimilar x r y = narrow $ N.makeSimilar (wide x) (wide r) (wide y)
