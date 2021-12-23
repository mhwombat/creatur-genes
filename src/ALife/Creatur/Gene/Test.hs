------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Test
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck test utilities.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Gene.Test
  (
    TestPattern(..),
    arb8BitDouble,
    arb8BitInt,
    prop_serialize_round_trippable,
    prop_genetic_round_trippable,
    prop_genetic_round_trippable2,
    prop_diploid_identity,
    prop_diploid_expressable,
    prop_diploid_readable,
    prop_show_read_round_trippable,
    prop_makeSimilar_works,
    prop_diff_can_be_0,
    prop_diff_can_be_1,
    prop_diff_is_symmetric,
    randomTestPattern,
    testPatternDiff,
    makeTestPatternSimilar,
    divvy
  ) where

import           ALife.Creatur.Gene.Numeric.UnitInterval (UIDouble, narrow,
                                                          wide)
import           ALife.Creatur.Gene.Numeric.Util         (forceToWord8,
                                                          scaleFromWord8,
                                                          scaleWord8ToInt)
import qualified ALife.Creatur.Genetics.BRGCWord8        as W8
import           ALife.Creatur.Genetics.Diploid          (Diploid, express)
import           ALife.Creatur.Util                      (fromEither)
import           Control.DeepSeq                         (NFData, deepseq)
import           Control.Monad.Random                    (Rand, RandomGen,
                                                          getRandom)
import           Control.Monad.State.Lazy                (runState)
import qualified Data.Datamining.Pattern.Numeric         as N
import           Data.Serialize                          (Serialize, decode,
                                                          encode)
import           Data.Word                               (Word8)
import           GHC.Generics                            (Generic)
import           Test.QuickCheck

-- | Returns a generator for 8-bit floating point values in the
--   specified range.
arb8BitDouble :: (Double, Double) -> Gen Double
arb8BitDouble interval = do
  x <- arbitrary :: Gen Word8
  return $ scaleFromWord8 interval x

-- | Returns a generator for 8-bit integer values in the
--   specified range.
arb8BitInt :: (Int, Int) -> Gen Int
arb8BitInt interval = do
  x <- arbitrary :: Gen Word8
  return $ scaleWord8ToInt interval x

-- | Verify that a value is not affected by serialisation and
--   deserialisation.
prop_serialize_round_trippable :: (Eq a, Serialize a) => a -> Bool
prop_serialize_round_trippable x = x' == Right x
  where bs = encode x
        x' = decode bs

-- | Verify that a value is not affected by encoding as a gene, then
--   decoding that gene.
prop_genetic_round_trippable :: (Eq g, W8.Genetic g, Show g) =>
  (g -> g -> Bool) -> g -> Bool
prop_genetic_round_trippable eq g = g' `eq` g && null leftover
  where x = W8.write g
        (result, (_, i, _)) = runState W8.get (x, 0, [])
        leftover = drop i x
        g' = fromEither (error "read returned Nothing") result

-- | Verify that genes are not affected by decoding and encoding.
prop_genetic_round_trippable2
  :: W8.Genetic g => Int -> [Word8] -> g -> Property
prop_genetic_round_trippable2 n xs dummy = length xs >= n
  ==> xs' == take n xs
  where Right g = W8.read xs
        xs' = W8.write (g `asTypeOf` dummy)

-- | Verify that two identical genes are expressed as that gene.
prop_diploid_identity :: Diploid g => (g -> g -> Bool) -> g -> Bool
prop_diploid_identity eq g = express g g `eq` g

-- | Verify a value is not affected by `show`ing and then `read`ing it.
prop_show_read_round_trippable
  :: (Read a, Show a) => (a -> a -> Bool) -> a -> Bool
prop_show_read_round_trippable eq x = (read . show $ x) `eq` x

-- | Verify that expressing a diploid gene does not cause an error.
prop_diploid_expressable
  :: (Diploid g, W8.Genetic g, NFData g) => g -> g -> Bool
prop_diploid_expressable a b = deepseq (express a b) True

-- | Verify that reading a diploid gene does not cause an error.
prop_diploid_readable
  :: (Diploid g, W8.Genetic g, NFData g)
    => g -> g -> Bool
prop_diploid_readable a b = deepseq (c `asTypeOf` a) True
  where ga = W8.write a
        gb = W8.write b
        (Right c) = W8.runDiploidReader W8.getAndExpress (ga, gb)

-- | Verify that `makeSimilar a b` returns a value that is no further
--   away from `b` than `a` was.
prop_makeSimilar_works
  :: (a -> a -> UIDouble) -> (a -> UIDouble -> a -> a) -> a -> UIDouble
    -> a -> Bool
prop_makeSimilar_works diff makeSimilar x r y = diffAfter <= diffBefore
  where diffBefore = diff x y
        y' = makeSimilar x r y
        diffAfter = diff x y'

prop_diff_can_be_0 :: (a -> a -> UIDouble) -> a -> Bool
prop_diff_can_be_0 diff x = diff x x == narrow 0

prop_diff_can_be_1 :: Bounded a => (a -> a -> UIDouble) -> a -> Bool
prop_diff_can_be_1 diff dummy
  = diff (minBound `asTypeOf` dummy) (maxBound `asTypeOf` dummy) == narrow 1

prop_diff_is_symmetric :: (a -> a -> UIDouble) -> a -> a -> Bool
prop_diff_is_symmetric diff x y = diff x y == diff y x

-- | A simple pattern that is useful for testing.
newtype TestPattern = TestPattern Word8
  deriving (Show, Read, Eq, Generic, Serialize, W8.Genetic, Diploid,
            NFData, Ord)

instance Arbitrary TestPattern where
  arbitrary = TestPattern <$> arbitrary

-- | Difference between two test patterns, expressed as a number
--   between 0 (identical) and 1 (maximally different).
testPatternDiff :: TestPattern -> TestPattern -> UIDouble
testPatternDiff (TestPattern x) (TestPattern y)
  = narrow $ abs (fromIntegral x - fromIntegral y) / 255

-- | @'makeTestPatternSimilar' target r x@ adjusts @x@ to make it
--   similar to @target@.
--   The amount of adjustment is controlled by @r@, where 0 <= @r@ <= 1.
--   If @r@=0, the result will be identical to @x@.
--   If @r@=1, the result will be identical @target@.
makeTestPatternSimilar
  :: TestPattern -> UIDouble -> TestPattern -> TestPattern
makeTestPatternSimilar (TestPattern target) r (TestPattern x)
    = TestPattern (forceToWord8 x'')
    where t' = fromIntegral target :: Double
          x' = fromIntegral x :: Double
          x'' = N.makeSimilar t' (wide r) x'

-- | Random pattern generator.
randomTestPattern :: RandomGen r => Rand r TestPattern
randomTestPattern = TestPattern <$> getRandom

-- | @'divvy' n k@ uses size @n@ to generate a vector of @k@ integers,
--   guaranteeing that the sum of the vector is less than @k@ or @n@,
--   whichever is greater.
divvy :: Int -> Int -> Gen [Int]
divvy n k = vectorOf k $ choose (1, n')
  where n' = max 1 (n `div` k)
