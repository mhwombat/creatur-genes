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
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Gene.Numeric.WeightsInternal where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Genetics.BRGCWord8        (Genetic, Reader, get,
                                                          put)
import           ALife.Creatur.Genetics.Diploid          (Diploid, express)
import           Control.DeepSeq                         (NFData)
import           Data.Serialize                          (Serialize)
import           GHC.Generics                            (Generic)
import           Test.QuickCheck                         (Arbitrary, arbitrary,
                                                          getSize, vectorOf)

-- | A sequence of weights for calculating weighted sums.
--   Do not export this constructor, export makeWeights instead.
newtype Weights a = Weights [a]
  deriving (Eq, Show, Read, Generic, Ord, Serialize, NFData)
  -- NOTE: Regarding Diploid instance, sum of weights will never be >1,
  -- because "express" chooses the smaller value.

instance (Fractional a, Real a) => Genetic (Weights a) where
  put (Weights ws) = do
    let ws' = map realToFrac ws :: [UI.Double]
    -- We won't have any infinite or NaN values, so using realToFrac is OK
    put ws'
  get = do
    ws <- get :: Reader (Either [String] [UI.Double])
    return $ fmap (makeWeights . map realToFrac) ws
    -- We won't have any infinite or NaN values, so using realToFrac is OK

instance (Diploid a, Fractional a, Ord a) => Diploid (Weights a) where
  express (Weights xs) (Weights ys) = makeWeights zs
    where zs = express xs ys

-- | Constructs a sequence of weights based on the input vector, but
--   normalised so that the sum of the weights is 1.
makeWeights :: (Fractional a, Ord a) => [a] -> Weights a
makeWeights [] = Weights []
makeWeights ws = Weights . normalise $ ws

-- | Number of weights in a sequence.
numWeights :: Weights a -> Int
numWeights (Weights xs) = length xs

-- | Calculates the weighted sum of a sequence of values.
--   If there are more values than weights, the excess values are
--   treated as if they have zero weight.
weightedSum :: Num a => Weights a -> [a] -> a
weightedSum (Weights ws) = sum . zipWith (*) ws

-- | Extract the weights from a @Weights@ object.
extractWeights :: Weights a -> [a]
extractWeights (Weights ws) = ws

-- | Returns the weight at a specified index in the sequence,
--   or zero if there is no weight at that index.
weightAt :: Num a => Weights a -> Int -> a
weightAt (Weights ws) n = if length ws > n
                           then ws !! n
                           else 0
-- Weights are short lists, so the call to length isn't too slow.

-- | Internal method
normalise :: (Fractional a, Num a, Ord a) => [a] -> [a]
normalise ws
  | any (< 0) ws = error "negative weight"
  | k == 0        = replicate n (1 / fromIntegral n)
  | otherwise    = tweak $ map scale ws
  where k = sum ws
        n = length ws
        scale w = w / k

-- | Internal method
tweak :: (Num a, Ord a) => [a] -> [a]
tweak (x:xs)
  | excess > 0 && x > excess = (x - excess) : xs
  | excess > 0              = 0 : tweak xs
  | otherwise               = x : xs
  where excess = max 0 $ s - 1
        s = sum (x:xs)
tweak [] = error "tweak should not have been called"

instance (Arbitrary a, Fractional a, Ord a) => Arbitrary (Weights a) where
  arbitrary = do
    n <- getSize
    makeWeights . map abs <$> vectorOf n arbitrary

