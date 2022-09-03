------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.NonNegativeQC
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Gene.Numeric.NonNegativeQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Numeric.NonNegative as NN
import           ALife.Creatur.Gene.Test                (prop_diploid_expressable,
                                                         prop_diploid_identity,
                                                         prop_diploid_readable,
                                                         prop_genetic_round_trippable,
                                                         prop_serialize_round_trippable,
                                                         prop_show_read_round_trippable)
import           Data.Datamining.Pattern                (prop_diff_btw_0_and_1,
                                                         prop_diff_can_be_0,
                                                         prop_diff_can_be_1,
                                                         prop_diff_symmetric,
                                                         prop_makeSimilar_can_leave_unchanged,
                                                         prop_makeSimilar_can_match_perfectly,
                                                         prop_makeSimilar_improves_similarity)
import           Test.Framework                         (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2   (testProperty)

approxEq :: NN.Double -> NN.Double -> Bool
approxEq x y = NN.doubleDiff x y < 1e-15

test :: Test
test = testGroup "ALife.Creatur.Gene.Numeric.NonNegativeQC"
  [
    testProperty "prop_serialize_round_trippable - NN.Double"
      (prop_serialize_round_trippable :: NN.Double -> Bool),
    testProperty "prop_show_read_round_trippable - NN.Double"
      (prop_show_read_round_trippable (==) :: NN.Double -> Bool),
    testProperty "prop_genetic_round_trippable - NN.Double"
      (prop_genetic_round_trippable (==) :: NN.Double -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - NN.Double"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> NN.Double -> Property),
    testProperty "prop_diploid_identity - NN.Double"
      (prop_diploid_identity (==) :: NN.Double -> Bool),
    testProperty "prop_diploid_expressable - NN.Double"
      (prop_diploid_expressable :: NN.Double -> NN.Double -> Bool),
    testProperty "prop_diploid_readable - NN.Double"
      (prop_diploid_readable :: NN.Double -> NN.Double -> Bool),

    testProperty "prop_serialize_round_trippable - [NN.Double]"
      (prop_serialize_round_trippable :: [NN.Double] -> Bool),
    testProperty "prop_show_read_round_trippable - [NN.Double]"
      (prop_show_read_round_trippable (==) :: [NN.Double] -> Bool),
    testProperty "prop_genetic_round_trippable - [NN.Double]"
      (prop_genetic_round_trippable (==) :: [NN.Double] -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - [NN.Double]"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> [NN.Double] -> Property),
    testProperty "prop_diploid_identity - [NN.Double]"
      (prop_diploid_identity (==) :: [NN.Double] -> Bool),
    testProperty "prop_diploid_expressable - [NN.Double]"
      (prop_diploid_expressable :: [NN.Double] -> [NN.Double] -> Bool),
    testProperty "prop_diploid_readable - [NN.Double]"
      (prop_diploid_readable :: [NN.Double] -> [NN.Double] -> Bool),

    testProperty "prop_diff_can_be_0"
      (prop_diff_can_be_0 NN.doubleDiff),
    testProperty "prop_diff_can_be_1"
      (prop_diff_can_be_1 NN.doubleDiff minBound maxBound),
    testProperty "prop_diff_btw_0_and_1"
      (prop_diff_btw_0_and_1 NN.doubleDiff),
    testProperty "prop_diff_symmetric"
      (prop_diff_symmetric NN.doubleDiff),

    testProperty "prop_makeSimilar_can_leave_unchanged"
      (prop_makeSimilar_can_leave_unchanged NN.makeDoubleSimilar (==)),
    testProperty "prop_makeSimilar_can_match_perfectly"
      (prop_makeSimilar_can_match_perfectly NN.makeDoubleSimilar
       (approxEq)),
    testProperty "prop_makeSimilar_improves_similarity"
      (prop_makeSimilar_improves_similarity
        NN.makeDoubleSimilar NN.doubleDiff)
  ]
