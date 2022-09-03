------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.UnitIntervalQC
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Gene.Numeric.UnitIntervalQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Test                 (prop_diploid_expressable,
                                                          prop_diploid_identity,
                                                          prop_diploid_readable,
                                                          prop_genetic_round_trippable,
                                                          prop_serialize_round_trippable,
                                                          prop_show_read_round_trippable)
import           Data.Datamining.Pattern                 (prop_diff_btw_0_and_1,
                                                          prop_diff_can_be_0,
                                                          prop_diff_can_be_1,
                                                          prop_diff_symmetric,
                                                          prop_makeSimilar_can_leave_unchanged,
                                                          prop_makeSimilar_can_match_perfectly,
                                                          prop_makeSimilar_improves_similarity)
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)

test :: Test
test = testGroup "ALife.Creatur.Gene.Numeric.UnitIntervalQC"
  [
    testProperty "prop_serialize_round_trippable - UI.Double"
      (prop_serialize_round_trippable :: UI.Double -> Bool),
    testProperty "prop_show_read_round_trippable - UI.Double"
      (prop_show_read_round_trippable (==) :: UI.Double -> Bool),
    testProperty "prop_genetic_round_trippable - UI.Double"
      (prop_genetic_round_trippable (==) :: UI.Double -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - UI.Double"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> UI.Double -> Property),
    testProperty "prop_diploid_identity - UI.Double"
      (prop_diploid_identity (==) :: UI.Double -> Bool),
    testProperty "prop_diploid_expressable - UI.Double"
      (prop_diploid_expressable :: UI.Double -> UI.Double -> Bool),
    testProperty "prop_diploid_readable - UI.Double"
      (prop_diploid_readable :: UI.Double -> UI.Double -> Bool),

    testProperty "prop_serialize_round_trippable - [UI.Double]"
      (prop_serialize_round_trippable :: [UI.Double] -> Bool),
    testProperty "prop_show_read_round_trippable - [UI.Double]"
      (prop_show_read_round_trippable (==) :: [UI.Double] -> Bool),
    testProperty "prop_genetic_round_trippable - [UI.Double]"
      (prop_genetic_round_trippable (==) :: [UI.Double] -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - [UI.Double]"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> [UI.Double] -> Property),
    testProperty "prop_diploid_identity - [UI.Double]"
      (prop_diploid_identity (==) :: [UI.Double] -> Bool),
    testProperty "prop_diploid_expressable - [UI.Double]"
      (prop_diploid_expressable :: [UI.Double] -> [UI.Double] -> Bool),
    testProperty "prop_diploid_readable - [UI.Double]"
      (prop_diploid_readable :: [UI.Double] -> [UI.Double] -> Bool),

    testProperty "prop_diff_can_be_0"
      (prop_diff_can_be_0 UI.doubleDiff),
    testProperty "prop_diff_can_be_1"
      (prop_diff_can_be_1 UI.doubleDiff minBound maxBound),
    testProperty "prop_diff_btw_0_and_1"
      (prop_diff_btw_0_and_1 UI.doubleDiff),
    testProperty "prop_diff_symmetric"
      (prop_diff_symmetric UI.doubleDiff),

    testProperty "prop_makeSimilar_can_leave_unchanged"
      (prop_makeSimilar_can_leave_unchanged UI.makeDoubleSimilar (==)),
    testProperty "prop_makeSimilar_can_match_perfectly"
      (prop_makeSimilar_can_match_perfectly UI.makeDoubleSimilar (==)),
    testProperty "prop_makeSimilar_improves_similarity"
      (prop_makeSimilar_improves_similarity
        UI.makeDoubleSimilar UI.doubleDiff)
  ]
