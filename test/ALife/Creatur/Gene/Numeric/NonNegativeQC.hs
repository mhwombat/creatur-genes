------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.NonNegativeQC
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
module ALife.Creatur.Gene.Numeric.NonNegativeQC
  (
    test
  ) where

import           ALife.Creatur.Gene.Numeric.NonNegative
import           ALife.Creatur.Gene.Numeric.UnitInterval (UIDouble)
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
test = testGroup "ALife.Creatur.Gene.Numeric.NonNegativeQC"
  [
    testProperty "prop_serialize_round_trippable - NNDouble"
      (prop_serialize_round_trippable :: NNDouble -> Bool),
    testProperty "prop_show_read_round_trippable - NNDouble"
      (prop_show_read_round_trippable (==) :: NNDouble -> Bool),
    testProperty "prop_genetic_round_trippable - NNDouble"
      (prop_genetic_round_trippable (==) :: NNDouble -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - NNDouble"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> NNDouble -> Property),
    testProperty "prop_diploid_identity - NNDouble"
      (prop_diploid_identity (==) :: NNDouble -> Bool),
    testProperty "prop_diploid_expressable - NNDouble"
      (prop_diploid_expressable :: NNDouble -> NNDouble -> Bool),
    testProperty "prop_diploid_readable - NNDouble"
      (prop_diploid_readable :: NNDouble -> NNDouble -> Bool),

    testProperty "prop_serialize_round_trippable - [NNDouble]"
      (prop_serialize_round_trippable :: [NNDouble] -> Bool),
    testProperty "prop_show_read_round_trippable - [NNDouble]"
      (prop_show_read_round_trippable (==) :: [NNDouble] -> Bool),
    testProperty "prop_genetic_round_trippable - [NNDouble]"
      (prop_genetic_round_trippable (==) :: [NNDouble] -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - [NNDouble]"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> [NNDouble] -> Property),
    testProperty "prop_diploid_identity - [NNDouble]"
      (prop_diploid_identity (==) :: [NNDouble] -> Bool),
    testProperty "prop_diploid_expressable - [NNDouble]"
      (prop_diploid_expressable :: [NNDouble] -> [NNDouble] -> Bool),
    testProperty "prop_diploid_readable - [NNDouble]"
      (prop_diploid_readable :: [NNDouble] -> [NNDouble] -> Bool),
    testProperty "prop_diff_can_be_1"
      (prop_diff_can_be_1 diff :: NNDouble -> Bool),
    testProperty "prop_diff_can_be_0"
      (prop_diff_can_be_0 diff :: NNDouble -> Bool),
    testProperty "prop_diff_is_symmetric"
      (prop_diff_is_symmetric diff :: NNDouble -> NNDouble -> Bool),
    testProperty "prop_makeSimilar_works - NNDouble"
      (prop_makeSimilar_works diff makeSimilar
        :: NNDouble -> UIDouble -> NNDouble -> Bool)
  ]
