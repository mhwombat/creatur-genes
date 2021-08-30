------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.WeightsInternal
-- Copyright   :  (c) 2015-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @Weights@ internals. Most developers
-- should use @Weights@ instead. This module is subject to change
-- without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ALife.Creatur.Gene.Numeric.WeightsInternal where

import ALife.Creatur.Genetics.BRGCWord8
    (Genetic, get)
import ALife.Creatur.Genetics.Diploid
    (Diploid, express)
import ALife.Creatur.Gene.Numeric.UnitInterval
    (UIDouble, normalise, uiDiff)
import Control.DeepSeq
    (NFData)
import Data.Serialize
    (Serialize)
import GHC.Generics
    (Generic)
import Test.QuickCheck
    (Arbitrary, arbitrary, Gen, sized, vectorOf)

-- | A sequence of weights for calculating weighted sums.
data Weights = Weights [UIDouble]
  deriving (Eq, Show, Read, Generic, Ord, Serialize, NFData)
  -- NOTE: Regarding Diploid instance, sum of weights will never be >1,
  -- because "express" chooses the smaller value.

instance Genetic Weights where
  -- use default put
  get = fmap (fmap Weights) get

instance Diploid Weights where
  express (Weights xs) (Weights ys) = makeWeights zs
    where zs = express xs ys

-- | Constructs a sequence of weights based on the input vector, but
--   normalised so that the sum of the weights is 1.
makeWeights :: [UIDouble] -> Weights
makeWeights [] = Weights []
makeWeights ws = Weights . normalise $ ws

-- | Number of weights in a sequence.
numWeights :: Weights -> Int
numWeights (Weights xs) = length xs

-- | Calculates the weighted sum of a sequence of values.
weightedSum :: Weights -> [UIDouble] -> UIDouble
weightedSum ws xs = sum $ zipWith (*) (toUIDoubles ws) xs

-- | Extract the weights from a @Weights@ object.
toUIDoubles :: Weights -> [UIDouble]
toUIDoubles (Weights xs) = xs

-- randomWeights :: RandomGen g => Int -> Rand g Weights
-- randomWeights n = do
--   xs <- fmap (take n) $ getRandomRs unitInterval
--   let s = sum xs
--   if s == 0
--     then return . Weights $ xs
--     else do
--       let ys = map (/s) xs
--       let zs = map (roundtripToWord8 unitInterval) $ ys
--       if sum zs > 1
--         then randomWeights n -- try again
--         else return $ Weights zs

-- | Returns the weight at a specified index in the sequence,
--   or zero if there is no weight at that index.
-- Weights are short lists, so the call to length isn't too
-- inefficient.
weightAt :: Weights -> Int -> UIDouble
weightAt w n = if length ws > n
               then ws !! n
               else 0
  where ws = toUIDoubles w

-- | Calculates the weighted difference between two sequences of
--   numbers.
--   Returns a number between 0 and 1.
--   A result of 0 indicates that the inputs are identical.
weightedUIVectorDiff
  :: Weights -> [UIDouble] -> [UIDouble] -> UIDouble
weightedUIVectorDiff ws xs ys
  = sum . zipWith (*) (toUIDoubles ws) $ zipWith uiDiff xs ys

-- | Generator for weights.
sizedArbWeights :: Int -> Gen Weights
sizedArbWeights n = fmap makeWeights $ vectorOf n arbitrary

-- sizedArbWeights :: Int -> Gen Weights
-- sizedArbWeights n = do
--   xs <- vectorOf n $ choose unitInterval
--   let s = sum xs
--   if s == 0
--     then return . Weights $ xs
--     else do
--       let ys = map (/s) xs
--       let zs = map (roundtripToWord8 unitInterval) $ ys
--       if sum zs > 1
--         then sizedArbWeights n -- try again
--         else return $ Weights zs

instance Arbitrary Weights where
  arbitrary = sized sizedArbWeights

