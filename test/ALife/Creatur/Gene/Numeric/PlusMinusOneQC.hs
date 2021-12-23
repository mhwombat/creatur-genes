------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.PlusMinusOneQC
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
module ALife.Creatur.Gene.Numeric.PlusMinusOneQC
  (
    test
  ) where

import           ALife.Creatur.Gene.Numeric.PlusMinusOne
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
test = testGroup "ALife.Creatur.Gene.Numeric.PlusMinusOneQC"
  [
    testProperty "prop_serialize_round_trippable - PM1Double"
      (prop_serialize_round_trippable :: PM1Double -> Bool),
    testProperty "prop_show_read_round_trippable - PM1Double"
      (prop_show_read_round_trippable (==) :: PM1Double -> Bool),
    testProperty "prop_genetic_round_trippable - PM1Double"
      (prop_genetic_round_trippable (==) :: PM1Double -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - PM1Double"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> PM1Double -> Property),
    testProperty "prop_diploid_identity - PM1Double"
      (prop_diploid_identity (==) :: PM1Double -> Bool),
    testProperty "prop_diploid_expressable - PM1Double"
      (prop_diploid_expressable :: PM1Double -> PM1Double -> Bool),
    testProperty "prop_diploid_readable - PM1Double"
      (prop_diploid_readable :: PM1Double -> PM1Double -> Bool),

    testProperty "prop_serialize_round_trippable - [PM1Double]"
      (prop_serialize_round_trippable :: [PM1Double] -> Bool),
    testProperty "prop_show_read_round_trippable - [PM1Double]"
      (prop_show_read_round_trippable (==) :: [PM1Double] -> Bool),
    testProperty "prop_genetic_round_trippable - [PM1Double]"
      (prop_genetic_round_trippable (==) :: [PM1Double] -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - [PM1Double]"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> [PM1Double] -> Property),
    testProperty "prop_diploid_identity - [PM1Double]"
      (prop_diploid_identity (==) :: [PM1Double] -> Bool),
    testProperty "prop_diploid_expressable - [PM1Double]"
      (prop_diploid_expressable :: [PM1Double] -> [PM1Double] -> Bool),
    testProperty "prop_diploid_readable - [PM1Double]"
      (prop_diploid_readable :: [PM1Double] -> [PM1Double] -> Bool),
    testProperty "prop_diff_can_be_1"
      (prop_diff_can_be_1 diff :: PM1Double -> Bool),
    testProperty "prop_diff_can_be_0"
      (prop_diff_can_be_0 diff :: PM1Double -> Bool),
    testProperty "prop_diff_is_symmetric"
      (prop_diff_is_symmetric diff :: PM1Double -> PM1Double -> Bool),
    testProperty "prop_makeSimilar_works - PM1Double"
      (prop_makeSimilar_works diff makeSimilar
        :: PM1Double -> UIDouble -> PM1Double -> Bool)
  ]
