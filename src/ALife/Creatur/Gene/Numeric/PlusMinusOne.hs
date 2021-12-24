------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.PlusMinusOne
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Numbers on the interval -1 to 1, inclusive.
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

module ALife.Creatur.Gene.Numeric.PlusMinusOne
  (
    PM1Double,
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
import           Data.Serialize                          (Serialize)
import           GHC.Generics                            (Generic)
import           System.Random                           (Random)
import           Test.QuickCheck                         (Arbitrary)

-- | A number on the interval -1 to 1, inclusive.
newtype PM1Double = PM1Double Double
  deriving (Eq, Ord, Generic)
  deriving anyclass (W8.Genetic)
  deriving newtype (NFData, Serialize)
  deriving (Show, Read, Random, Arbitrary, Diploid, Num,
            Fractional, Floating, Real)
    via (UseNarrow PM1Double)

instance Bounded PM1Double where
  minBound = PM1Double (-1)
  maxBound = PM1Double 1

instance Narrow PM1Double where
  type BaseType PM1Double = Double
  unsafeConstructor = PM1Double
  wide (PM1Double x) = x
