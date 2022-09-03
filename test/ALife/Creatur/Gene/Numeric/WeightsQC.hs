------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.WeightsQC
-- Copyright   :  (c) 2015-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Gene.Numeric.WeightsQC
  (
    test
  ) where

import           ALife.Creatur.Gene.Numeric.UnitIntervalQC  ()
import           ALife.Creatur.Gene.Numeric.WeightsInternal
import           ALife.Creatur.Gene.Test
import qualified Numeric.ApproxEq                           as N
import           Test.Framework                             (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2       (testProperty)
import           Test.QuickCheck.Counterexamples

prop_sum_of_weights_is_1 :: Weights Double -> Property
prop_sum_of_weights_is_1 (Weights ws)
  = not (null ws) ==> sum ws <= 1 && (sum ws - 1) < 1e-10

prop_weights_are_positive :: Weights Double -> Bool
prop_weights_are_positive (Weights ws) = all (>= 0) ws

prop_weighted_sum_in_range :: Weights Double -> [Double] -> Bool
prop_weighted_sum_in_range ws xs = seq (weightedSum ws xs) True

prop_weights_are_normalised :: Weights Double -> Property
prop_weights_are_normalised (Weights ws)
  = not (null ws) ==> sum ws <= 1 && N.within 10 1 (sum ws)

-- Both get and express (re)normalise weights, which can result in
-- small differences even if the weights are already normalised.
equiv :: Weights Double -> Weights Double -> Bool
equiv (Weights xs) (Weights ys)
  = length xs == length ys && and (zipWith (N.within 100000) xs ys)

test :: Test
test = testGroup "ALife.Creatur.Gene.Numeric.WeightsQC"
  [
    testProperty "prop_serialize_round_trippable - Weights"
      (prop_serialize_round_trippable :: Weights Double -> Bool),
    testProperty "prop_genetic_round_trippable - Weights"
      (prop_genetic_round_trippable equiv :: Weights Double -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Weights"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> Weights -> Property),
    testProperty "prop_diploid_identity - Weights"
      (prop_diploid_identity equiv :: Weights Double -> Bool),
    testProperty "prop_show_read_round_trippable - Weights"
      (prop_show_read_round_trippable (==) :: Weights Double -> Bool),
    testProperty "prop_diploid_expressable - Weights"
      (prop_diploid_expressable :: Weights Double -> Weights Double -> Bool),
    testProperty "prop_diploid_readable - Weights"
      (prop_diploid_readable :: Weights Double -> Weights Double -> Bool),
    testProperty "prop_sum_of_weights_is_1"
      prop_sum_of_weights_is_1,
    testProperty "prop_weights_are_positive"
      prop_weights_are_positive,
    testProperty "prop_weighs_are_normalised"
      prop_weights_are_normalised,
    testProperty "prop_weighted_sum_in_range"
      prop_weighted_sum_in_range
  ]
