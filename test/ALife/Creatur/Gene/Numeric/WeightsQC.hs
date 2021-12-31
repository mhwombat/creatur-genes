------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.WeightsQC
-- Copyright   :  (c) 2015-2021 Amy de Buitléir
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
    test,
    sizedArbWeights
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval    as UI
import           ALife.Creatur.Gene.Numeric.UnitIntervalQC  ()
import           ALife.Creatur.Gene.Numeric.WeightsInternal
import           ALife.Creatur.Gene.Test
import qualified Numeric.ApproxEq                           as N
import           Test.Framework                             (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2       (testProperty)
import           Test.QuickCheck

prop_sum_of_weights_is_1 :: Weights -> Property
prop_sum_of_weights_is_1 w
  = not (null ws) ==> sum ws <= 1 && (sum ws - 1) < 1e-10
  where ws = map UI.wide $ toUIDoubles w

prop_weights_are_positive :: Weights -> Bool
prop_weights_are_positive w = (all ((>= 0) . UI.wide) . toUIDoubles) w

prop_weighted_sum_in_range :: Weights -> [UI.Double] -> Bool
prop_weighted_sum_in_range ws xs = seq (weightedSum ws xs) True

prop_weights_are_normalised :: Weights -> Property
prop_weights_are_normalised ws
  = not (null xs) ==> sum xs <= 1 && N.within 100 1 (sum xs)
  where xs = map UI.wide $ toUIDoubles ws

-- The express function in Diploid normalises the weights, so the identity may
-- not hold exactly.
equiv :: Weights -> Weights -> Bool
equiv (Weights xs) (Weights ys)
  = length xs == length ys
      && and (zipWith (N.within 100000) (map UI.wide xs) (map UI.wide ys))

test :: Test
test = testGroup "ALife.Creatur.Gene.Numeric.WeightsQC"
  [
    testProperty "prop_serialize_round_trippable - Weights"
      (prop_serialize_round_trippable :: Weights -> Bool),
    testProperty "prop_genetic_round_trippable - Weights"
      (prop_genetic_round_trippable (==) :: Weights -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Weights"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> Weights -> Property),
    testProperty "prop_diploid_identity - Weights"
      (prop_diploid_identity equiv :: Weights -> Bool),
    testProperty "prop_show_read_round_trippable - Weights"
      (prop_show_read_round_trippable (==) :: Weights -> Bool),
    testProperty "prop_diploid_expressable - Weights"
      (prop_diploid_expressable :: Weights -> Weights -> Bool),
    testProperty "prop_diploid_readable - Weights"
      (prop_diploid_readable :: Weights -> Weights -> Bool),
    testProperty "prop_sum_of_weights_is_1"
      prop_sum_of_weights_is_1,
    testProperty "prop_weights_are_positive"
      prop_weights_are_positive,
    testProperty "prop_weighs_are_normalised"
      prop_weights_are_normalised,
    testProperty "prop_weighted_sum_in_range"
      prop_weighted_sum_in_range
  ]
