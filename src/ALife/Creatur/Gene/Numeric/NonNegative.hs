------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.NonNegative
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

module ALife.Creatur.Gene.Numeric.NonNegative
  (
    NNDouble,
    narrow,
    wide,
    crop,
    diff,
    makeSimilar
  ) where

import           ALife.Creatur.Gene.Numeric.Narrow       (BaseType, Narrow,
                                                          UseNarrow (..), crop,
                                                          narrow,
                                                          unsafeConstructor,
                                                          wide)
import           ALife.Creatur.Gene.Numeric.UnitInterval (diff, makeSimilar)
import qualified ALife.Creatur.Genetics.BRGCWord8        as W8
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           Control.DeepSeq                         (NFData)
import qualified Data.Datamining.Pattern.Numeric         as N
import           Data.Serialize                          (Serialize)
import           GHC.Generics                            (Generic)
import           System.Random                           (Random)
import           Test.QuickCheck                         (Arbitrary)

-- | A non-negative number
newtype NNDouble = NNDouble Double
  deriving (Eq, Ord, Generic)
  deriving anyclass (W8.Genetic)
  deriving newtype (NFData, Serialize)
  deriving (Show, Read, Random, Arbitrary, Diploid, Num,
            Fractional, Floating, Real)
    via (UseNarrow NNDouble)

instance Bounded NNDouble where
  minBound = NNDouble 0
  maxBound = NNDouble (N.maxDouble / 2)
    -- division by two ensures that diploid expression stays in bounds

instance Narrow NNDouble where
  type BaseType NNDouble = Double
  unsafeConstructor = NNDouble
  wide (NNDouble x) = x
