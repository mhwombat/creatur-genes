------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
import           ALife.Creatur.Gene.Numeric.NonNegativeQC  (test)
import           ALife.Creatur.Gene.Numeric.PlusMinusOneQC (test)
import           ALife.Creatur.Gene.Numeric.UnitIntervalQC (test)
import           ALife.Creatur.Gene.Numeric.UtilQC         (test)
import           ALife.Creatur.Gene.Numeric.WeightsQC      (test)
import           ALife.Creatur.Gene.TestQC                 (test)

import           Test.Framework                            as TF (Test,
                                                                  defaultMain)

tests :: [TF.Test]
tests =
  [
    -- In increasing order of complexity
    ALife.Creatur.Gene.TestQC.test,
    ALife.Creatur.Gene.Numeric.UtilQC.test,
    ALife.Creatur.Gene.Numeric.UnitIntervalQC.test,
    ALife.Creatur.Gene.Numeric.NonNegativeQC.test,
    ALife.Creatur.Gene.Numeric.PlusMinusOneQC.test,
    ALife.Creatur.Gene.Numeric.WeightsQC.test
  ]

main :: IO ()
main = defaultMain tests
