------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.TestQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Gene.TestQC
  (
    test
  ) where

import           ALife.Creatur.Gene.Test
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.QuickCheck                       hiding
    (classify, maxSize)

test :: Test
test = testGroup "ALife.Creatur.Gene.TestQC"
  [
    testProperty "prop_serialize_round_trippable - TestPattern"
      (prop_serialize_round_trippable :: TestPattern -> Property),
    testProperty "prop_genetic_round_trippable - TestPattern"
      (prop_genetic_round_trippable (==) :: TestPattern -> Property),
    -- testProperty "prop_genetic_round_trippable2 - TestPattern"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestPattern -> Property),
    testProperty "prop_diploid_identity - TestPattern"
      (prop_diploid_identity (==) :: TestPattern -> Property),
    testProperty "prop_show_read_round_trippable - TestPattern"
      (prop_show_read_round_trippable (==) :: TestPattern -> Property),
    testProperty "prop_diploid_expressable - TestPattern"
      (prop_diploid_expressable :: TestPattern -> TestPattern -> Property),
    testProperty "prop_diploid_readable - TestPattern"
      (prop_diploid_readable :: TestPattern -> TestPattern -> Property)
  ]
