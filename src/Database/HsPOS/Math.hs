{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- | Module: Database.HsPOS.Math
-- License: BSD3
-- Stability: Unstable
-- Portability: GHC
-- Description: Math functions for HsPOS.
-- This module includes some math functions that are not included in the
-- standard Haskell libraries, but are needed for the HsPOS project.
-- They are primarily used for statistical calculations, however, they are not
-- specific to statistics or to HsPOS. Not all of them are used in the current
-- version of HsPOS, some were used in previous versions. They are included
-- here for completeness and for future use. Everything in this module is marked
-- as INLINE for performance. Additionally, this module makes use of bang patterns
-- for none-list values.
module Database.HsPOS.Math where

import Data.Vector (Vector)
import Data.Vector qualified as V

-- | Euler's constant - ie. "e".
eulersNumber :: Double
eulersNumber = exp 1

-- | imaginary unit
i :: Double
i = sqrt (-1)

-- | Calculates the variation for a list of numbers.
variance :: Floating a => Vector a -> a
-- Explanation for this perhaps non-obvious code:
-- 1. Calculate the mean of the list (avg = average xs)
-- 2. Composite (^2) and (-) onto the mean, then map (<$>) over the list
-- 3. Apply the composite to the list
-- 4. Return the result
variance xs = average $ (** 2) . (-) avg <$> xs
  where
    !avg = average xs
{-# INLINE variance #-}

-- | Calculate the std. deviation of a list of Floating numbers. Since standard
--   deviation is merely the root of variation, stddev is just a composite of
--   sqrt and variance.
stddev :: (Floating a, Ord a) => Vector a -> a
-- eta-reduced.
stddev = sqrt . variance
{-# INLINE stddev #-}

-- | Calculate the average of a list of Floating numbers.
average :: Floating a => Vector a -> a
-- eta reduced.
average = (/) <$> sum <*> realToFrac . V.length
{-# INLINE average #-}

-- | Calculate the Covariance of two lists of Floating numbers.
-- they must be of the same length. If they are not, the result is undefined.
covariance :: Floating a => Vector a -> Vector a -> a
-- You might be looking at this and thinking "What the **** is this?".
-- Here's an explanation of sorts:
-- First, the averages are calculated. This is put in a where clause so that it
-- is only calculated once. (referential transparency should guarantee that
-- this is the case, but I'm not sure if it does)
--
-- The function body might look a little strange, but it's actually quite
-- elegant. Remember, the . operator is composition - we can think of it as
-- a pipe. for example:
-- (/ len) . sum [a list] is like:
-- sum the list => divide by len.
--
-- Now, the covariance is calculated. Here's a breakdown of the code:
-- zipWith - combine two lists using a function (in this case, multiplication)
-- the two lists are
-- - (avgX -) <$> xs - the difference between each element of xs and avgX
--           (<$> is fmap, a generalization of map which works on any Functor).
-- - (avgY -) <$> ys - the difference between each element of ys and avgY
-- sum - sum the list
-- length xs - get the length of xs (the length of ys should the same)
-- realToFrac - convert the length of the list to a Floating number (GHC infers that it is of type a)
-- (/) - divide the sum by the length
-- And that gives us the covariance!
covariance xs ys =
  (/ len)
    . sum
    $ V.zipWith (*) ((avgX -) <$> xs) ((avgY -) <$> ys)
  where
    !len = realToFrac . V.length $ xs
    !avgX = average xs
    !avgY = average ys
{-# INLINE covariance #-}

-- | Calculate the Correlation Coefficient of two lists of Floating point numbers.
--    They must be of the same length. If they are not, the result is undefined.
correlation :: (Floating a, Ord a) => Vector a -> Vector a -> a
-- Self explanatory.
correlation xs ys = covariance xs ys / (stddev xs * stddev ys)
{-# INLINE correlation #-}

-- | Calculate the absolute difference between two numbers.
--   Shorthand for abs (x - y)
absDiff :: Num a => a -> a -> a
-- eta reduced.
absDiff = (abs .) . (-)
{-# INLINE absDiff #-}

-- | leastSq returns a function (Floating a => a -> a) which returns the estimated
--    value of y for a given x. It uses the least squares method to calculate the
--    function. The function takes two lists of Floating point numbers. They must
--    be of the same length. If they are not, the result is undefined, as leastSq
--    operates on paired data.
leastSq :: Floating a => Vector a -> Vector a -> (a -> a)
leastSq xs ys = \x -> m * x + b
  where
    !len = realToFrac . V.length $ xs
    !m =
      -- Slope
      ( len
          * V.sum (V.zipWith (*) xs ys)
          - sum xs * sum ys
      )
        / (len * V.sum ((** 2) <$> xs) - sum xs ** 2)
    !b =
      -- y-intercept
      (V.sum ys - m * sum xs)
        / len
