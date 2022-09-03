------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.PlusMinusOneQC
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Gene.Numeric.PlusMinusOneQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
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

approxEq :: PM1.Double -> PM1.Double -> Bool
approxEq x y = PM1.doubleDiff x y < 1e-15

test :: Test
test = testGroup "ALife.Creatur.Gene.Numeric.PlusMinusOneQC"
  [
    testProperty "prop_serialize_round_trippable - PM1.Double"
      (prop_serialize_round_trippable :: PM1.Double -> Bool),
    testProperty "prop_show_read_round_trippable - PM1.Double"
      (prop_show_read_round_trippable (==) :: PM1.Double -> Bool),
    testProperty "prop_genetic_round_trippable - PM1.Double"
      (prop_genetic_round_trippable (==) :: PM1.Double -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - PM1.Double"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> PM1.Double -> Property),
    testProperty "prop_diploid_identity - PM1.Double"
      (prop_diploid_identity (==) :: PM1.Double -> Bool),
    testProperty "prop_diploid_expressable - PM1.Double"
      (prop_diploid_expressable :: PM1.Double -> PM1.Double -> Bool),
    testProperty "prop_diploid_readable - PM1.Double"
      (prop_diploid_readable :: PM1.Double -> PM1.Double -> Bool),

    testProperty "prop_serialize_round_trippable - [PM1.Double]"
      (prop_serialize_round_trippable :: [PM1.Double] -> Bool),
    testProperty "prop_show_read_round_trippable - [PM1.Double]"
      (prop_show_read_round_trippable (==) :: [PM1.Double] -> Bool),
    testProperty "prop_genetic_round_trippable - [PM1.Double]"
      (prop_genetic_round_trippable (==) :: [PM1.Double] -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - [PM1.Double]"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> [PM1.Double] -> Property),
    testProperty "prop_diploid_identity - [PM1.Double]"
      (prop_diploid_identity (==) :: [PM1.Double] -> Bool),
    testProperty "prop_diploid_expressable - [PM1.Double]"
      (prop_diploid_expressable :: [PM1.Double] -> [PM1.Double] -> Bool),
    testProperty "prop_diploid_readable - [PM1.Double]"
      (prop_diploid_readable :: [PM1.Double] -> [PM1.Double] -> Bool),

    testProperty "prop_diff_can_be_0"
      (prop_diff_can_be_0 PM1.doubleDiff),
    testProperty "prop_diff_can_be_1"
      (prop_diff_can_be_1 PM1.doubleDiff minBound maxBound),
    testProperty "prop_diff_btw_0_and_1"
      (prop_diff_btw_0_and_1 PM1.doubleDiff),
    testProperty "prop_diff_symmetric"
      (prop_diff_symmetric UI.doubleDiff),

    testProperty "prop_makeSimilar_can_leave_unchanged"
      (prop_makeSimilar_can_leave_unchanged PM1.makeDoubleSimilar (==)),
    testProperty "prop_makeSimilar_can_match_perfectly"
      (prop_makeSimilar_can_match_perfectly PM1.makeDoubleSimilar
       (approxEq)),
    testProperty "prop_makeSimilar_improves_similarity"
      (prop_makeSimilar_improves_similarity
        PM1.makeDoubleSimilar PM1.doubleDiff)
  ]
