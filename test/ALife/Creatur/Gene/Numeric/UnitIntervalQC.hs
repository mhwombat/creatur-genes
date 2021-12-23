------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.UnitIntervalQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Gene.Numeric.UnitIntervalQC
  (
    test
  ) where

import           ALife.Creatur.Gene.Numeric.UnitInterval
import           ALife.Creatur.Gene.Test                 (prop_diff_can_be_0,
                                                          prop_diff_can_be_1,
                                                          prop_diff_is_symmetric,
                                                          prop_diploid_expressable,
                                                          prop_diploid_identity,
                                                          prop_diploid_readable,
                                                          prop_genetic_round_trippable,
                                                          prop_makeSimilar_works,
                                                          prop_serialize_round_trippable,
                                                          prop_show_read_round_trippable)
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)

test :: Test
test = testGroup "ALife.Creatur.Gene.Numeric.UnitIntervalQC"
  [
    testProperty "prop_serialize_round_trippable - UIDouble"
      (prop_serialize_round_trippable :: UIDouble -> Bool),
    testProperty "prop_show_read_round_trippable - UIDouble"
      (prop_show_read_round_trippable (==) :: UIDouble -> Bool),
    testProperty "prop_genetic_round_trippable - UIDouble"
      (prop_genetic_round_trippable (==) :: UIDouble -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - UIDouble"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> UIDouble -> Property),
    testProperty "prop_diploid_identity - UIDouble"
      (prop_diploid_identity (==) :: UIDouble -> Bool),
    testProperty "prop_diploid_expressable - UIDouble"
      (prop_diploid_expressable :: UIDouble -> UIDouble -> Bool),
    testProperty "prop_diploid_readable - UIDouble"
      (prop_diploid_readable :: UIDouble -> UIDouble -> Bool),

    testProperty "prop_serialize_round_trippable - [UIDouble]"
      (prop_serialize_round_trippable :: [UIDouble] -> Bool),
    testProperty "prop_show_read_round_trippable - [UIDouble]"
      (prop_show_read_round_trippable (==) :: [UIDouble] -> Bool),
    testProperty "prop_genetic_round_trippable - [UIDouble]"
      (prop_genetic_round_trippable (==) :: [UIDouble] -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - [UIDouble]"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> [UIDouble] -> Property),
    testProperty "prop_diploid_identity - [UIDouble]"
      (prop_diploid_identity (==) :: [UIDouble] -> Bool),
    testProperty "prop_diploid_expressable - [UIDouble]"
      (prop_diploid_expressable :: [UIDouble] -> [UIDouble] -> Bool),
    testProperty "prop_diploid_readable - [UIDouble]"
      (prop_diploid_readable :: [UIDouble] -> [UIDouble] -> Bool),
    testProperty "prop_diff_can_be_1"
      (prop_diff_can_be_1 diff :: UIDouble -> Bool),
    testProperty "prop_diff_can_be_0"
      (prop_diff_can_be_0 diff :: UIDouble -> Bool),
    testProperty "prop_diff_is_symmetric"
      (prop_diff_is_symmetric diff :: UIDouble -> UIDouble -> Bool),
    testProperty "prop_makeSimilar_works - UIDouble"
      (prop_makeSimilar_works diff makeSimilar
        :: UIDouble -> UIDouble -> UIDouble -> Bool)
  ]
