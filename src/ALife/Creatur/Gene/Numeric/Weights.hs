------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.Weights
-- Copyright   :  (c) 2015-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Weights used to calculate weighted sums.
--
------------------------------------------------------------------------
module ALife.Creatur.Gene.Numeric.Weights
  (
    Weights,
    makeWeights,
    weightedSum,
    weightAt,
    extractWeights,
    numWeights
  ) where

import           ALife.Creatur.Gene.Numeric.WeightsInternal
