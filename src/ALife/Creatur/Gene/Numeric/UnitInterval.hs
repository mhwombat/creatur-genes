------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.UnitInterval
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
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
    Double,
    narrow,
    wide,
    crop,
    doubleDiff,
    makeDoubleSimilar
  ) where

import           Prelude                           hiding (Double)
import qualified Prelude

import           ALife.Creatur.Gene.Numeric.Narrow (BaseType, Narrow,
                                                    UseNarrow (..), crop,
                                                    narrow, unsafeConstructor,
                                                    wide)
import qualified ALife.Creatur.Genetics.BRGCWord8  as W8
import           ALife.Creatur.Genetics.Diploid    (Diploid)
import           Control.DeepSeq                   (NFData)
import           Data.Datamining.Pattern.Numeric   (boundedFractionalDiff,
                                                    makeRealFracSimilar)
import           Data.Serialize                    (Serialize)
import           GHC.Generics                      (Generic)
import           System.Random                     (Random)
import           Test.QuickCheck                   (Arbitrary)

-- | A number on the unit interval
newtype Double = Double Prelude.Double
  deriving (Eq, Ord, Generic)
  deriving anyclass (W8.Genetic)
  deriving newtype (NFData, Serialize, Diploid)
  deriving (Show, Read, Random, Arbitrary, Num, Fractional, Floating, Real)
    via (UseNarrow Double)

instance Bounded Double where
  minBound = Double 0
  maxBound = Double 1

instance Narrow Double where
  type BaseType Double = Prelude.Double
  unsafeConstructor = Double
  wide (Double x) = x

doubleDiff :: Double -> Double -> Double
doubleDiff = boundedFractionalDiff

makeDoubleSimilar :: Double -> Double -> Double -> Double
makeDoubleSimilar = makeRealFracSimilar
