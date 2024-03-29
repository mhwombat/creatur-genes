------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Gene.Numeric.Util
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions that don't fit anywhere else.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module ALife.Creatur.Gene.Numeric.Util
  (
    unitInterval,
    inRange,
    enforceRange,
    scaleToWord8,
    scaleToWord16,
    scaleToWord64,
    scaleFromWord8,
    scaleFromWord16,
    scaleFromWord64,
    forceToWord8,
    scaleIntToWord8,
    scaleWord8ToInt,
    forceIntToWord8,
    word8ToInt,
    forceIntToWord16,
    word16ToInt,
    intersection,
    unitVectors
  ) where

import           Data.Word (Word16, Word64, Word8)

-- | The range [0,1], endpoints included.
unitInterval :: Num a => (a, a)
unitInterval = (0,1)

-- | Returns @True@ if the value is in the specified range, endpoints
--   included; returns @False@ otherwise.
inRange :: Ord c => (c, c) -> c -> Bool
inRange (a,b) c = c >= a && c <= b

-- | If the value is outside the specified range, endpoints included,
--   return the nearest endpoint; otherwise, return the value.
enforceRange :: Ord c => (c, c) -> c -> c
enforceRange (a,b) = max a . min b

-- | @'scaleToWord8' (a, b) x@ scales @x@ to create a @Word8@ value,
--   such that @scaleToWord8 (a, b) a@ = 0 and
--   @scaleToWord8 (a, b) b@ = 255.
scaleToWord8 :: RealFrac a => (a, a) -> a -> Word8
scaleToWord8 (a, b) x = round (m*(x-a)/(b-a))
  where m = fromIntegral (maxBound :: Word8)

-- | @'scaleToWord16' (a, b) x@ scales @x@ to create a @Word16@ value,
--   such that @scaleToWord16 (a, b) a@ = 0 and
--   @scaleToWord16 (a, b) b@ = 255.
scaleToWord16 :: RealFrac a => (a, a) -> a -> Word16
scaleToWord16 (a, b) x = round (m*(x-a)/(b-a))
  where m = fromIntegral (maxBound :: Word16)

-- | @'scaleToWord64' (a, b) x@ scales @x@ to create a @Word64@ value,
--   such that @scaleToWord64 (a, b) a@ = 0 and
--   @scaleToWord64 (a, b) b@ = 255.
scaleToWord64 :: RealFrac a => (a, a) -> a -> Word64
scaleToWord64 (a, b) x = round (m*(x-a)/(b-a))
  where m = 18446744073709551615
  -- where m = fromIntegral (maxBound :: Word64)

-- | This function is the inverse of @'scaleToWord8'@.
--   @'scaleFromWord8' (a, b) 0@ = a and
--   @'scaleFromWord8' (a, b) 255@ = b.
scaleFromWord8 :: Fractional a => (a, a) -> Word8 -> a
scaleFromWord8 (a, b) x = a + fromIntegral x * (b-a)/m
  where m = fromIntegral (maxBound :: Word8)

-- | This function is the inverse of @'scaleToWord16'@.
--   @'scaleFromWord16' (a, b) 0@ = a and
--   @'scaleFromWord16' (a, b) 255@ = b.
scaleFromWord16 :: Fractional a => (a, a) -> Word16 -> a
scaleFromWord16 (a, b) x = a + fromIntegral x * (b-a)/m
  where m = fromIntegral (maxBound :: Word16)

-- | This function is the inverse of @'scaleToWord64'@.
--   @'scaleFromWord64' (a, b) 0@ = a and
--   @'scaleFromWord64' (a, b) 255@ = b.
scaleFromWord64 :: Fractional a => (a, a) -> Word64 -> a
scaleFromWord64 (a, b) x = a + fromIntegral x * (b-a)/m
  where m = 18446744073709551615
  -- where m = fromIntegral (maxBound :: Word64)


-- | @'forceToWord8' x@ returns 255 or the rounded value of @x@,
--   whichever is smaller.
forceToWord8 :: (Num a, Ord a, RealFrac a) => a -> Word8
forceToWord8 = round . min 255

-- | @'scaleIntToWord8' (a, b) x@ scales @x@ to create a @Word8@ value,
--   such that @scaleIntToWord8 (a, b) a@ = 0 and
--   @scaleIntToWord8 (a, b) b@ = 255.
scaleIntToWord8 :: (Int, Int) -> Int -> Word8
scaleIntToWord8 (a, b) x = round (255*(x'-a')/(b'-a'))
  where a' = fromIntegral a :: Double
        b' = fromIntegral b :: Double
        x' = fromIntegral x :: Double

-- | This function is the inverse of @'scaleIntToWord8'@.
--   @'scaleWord8ToInt' (a, b) 0@ = a and
--   @'scaleWord8ToInt' (a, b) 255@ = b.
scaleWord8ToInt :: (Int, Int) -> Word8 -> Int
scaleWord8ToInt (a, b) x = a + fromIntegral x * (b-a) `div` 255

-- | @'forceIntToWord8' x@ returns 255 or @x@, whichever is smaller.
--   TODO: How should negative values be handled?
forceIntToWord8 :: Int -> Word8
forceIntToWord8 = fromIntegral . min maxBound

-- | Converts a @Word8@ to an @Int@.
word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

-- roundtripToWord8 :: (Fractional a, RealFrac a) => (a, a) -> a -> a
-- roundtripToWord8 interval = scaleFromWord8 interval . scaleToWord8 interval

-- | @'forceIntToWord8' x@ returns 65535 or @x@, whichever is smaller.
--   TODO: How should negative values be handled?
forceIntToWord16 :: Int -> Word16
forceIntToWord16 = fromIntegral . min maxBound

-- | Converts a @Word16@ to an @Int@.
word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

proper :: Ord a => (a, a) -> (a, a)
proper (x, y) = if x <= y then (x, y) else (y, x)

-- | Returns the largest interval that satisfies both of the supplied
--   intervals. If either interval is reversed (i.e., the start of the
--   interval is >= the end), the endpoints will be swapped before
--   proceeding. If the supplied intervals do not overlap, the first
--   interval is used.
intersection :: Ord a => (a, a) -> (a, a) -> (a, a)
intersection (a, b) (c, d)
  | b' < c' || d' < a' = (a', b')
  | otherwise         = (max a' c', min b' d')
  where (a', b') = proper (a, b)
        (c', d') = proper (c, d)

-- random8BitDouble :: (Double, Double) -> m Double
-- random8BitDouble interval = do
--   x <- getRandom
--   return $ scaleFromWord8 interval x

-- | @'unitVectors' n@ returns a set of unit vectors of dimension @n@.
--   For example, @unitVectors 3@ returns @[[1,0,0],[0,1,0],[0,0,1]]@.
unitVectors :: Num a => Int -> [[a]]
unitVectors n = map f [0..n-1]
  where f k = replicate k 0 ++ 1 : replicate (n-k-1) 0
