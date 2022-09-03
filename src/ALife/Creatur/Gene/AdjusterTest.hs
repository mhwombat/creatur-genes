------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.AdjusterTest
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck test utilities.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Gene.AdjusterTest
  (
    prop_diff_can_be_0,
    prop_diff_can_be_1,
    prop_diff_is_symmetric,
    prop_makeSimilar_improves_similarity,
    prop_zero_adjustment_makes_no_change,
    prop_full_adjustment_gives_perfect_match
  ) where

import           Data.Datamining.Clustering.SGM4 (Adjuster, MetricType,
                                                  PatternType, difference,
                                                  makeSimilar)

prop_diff_can_be_0
  :: (Adjuster a, Eq (MetricType a), Num (MetricType a))
  => a -> PatternType a -> Bool
prop_diff_can_be_0 a x = difference a x x == 0

prop_diff_can_be_1
  :: (Adjuster a, Bounded (PatternType a),
     Eq (MetricType a), Num (MetricType a))
  => a -> PatternType a -> Bool
prop_diff_can_be_1 a dummy
  = difference a (minBound `asTypeOf` dummy) (maxBound `asTypeOf` dummy) == 1

prop_diff_is_symmetric
  :: (Adjuster a, Eq (MetricType a))
  => a -> PatternType a -> PatternType a -> Bool
prop_diff_is_symmetric a x y = difference a x y == difference a y x

prop_makeSimilar_improves_similarity
  :: (Adjuster a, Eq (MetricType a), Ord (MetricType a))
  => a -> PatternType a -> MetricType a -> PatternType a -> Bool
prop_makeSimilar_improves_similarity a x r y = diffAfter <= diffBefore
  where diffBefore = difference a x y
        y' = makeSimilar a x r y
        diffAfter = difference a x y'

prop_zero_adjustment_makes_no_change
  :: (Adjuster a, Eq (MetricType a), Num(MetricType a), Ord (MetricType a))
  => (PatternType a -> PatternType a -> Bool) -> a -> PatternType a
  -> PatternType a -> Bool
prop_zero_adjustment_makes_no_change eq a x y = y' `eq` y
  where y' = makeSimilar a x 0 y

prop_full_adjustment_gives_perfect_match
  :: (Adjuster a, Eq (MetricType a), Num(MetricType a), Ord (MetricType a))
  => (PatternType a -> PatternType a -> Bool) -> a -> PatternType a
  -> PatternType a -> Bool
prop_full_adjustment_gives_perfect_match eq a x y = y' `eq` x
  where y' = makeSimilar a x 1 y
