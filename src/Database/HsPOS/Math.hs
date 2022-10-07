{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module includes some math functions that are not included in the
--    standard Haskell libraries, but are needed for the HsPOS project.
--    They are primarily used for statistical calculations, however, they are not
--    specific to statistics or to HsPOS. Not all of them are used in the current
--    version of HsPOS, some were used in previous versions. They are included
--    here for completeness and for future use. Everything in this module is marked
--    as INLINE for performance. Additionally, this module makes use of bang patterns
--    for none-list values.
module Database.HsPOS.Math where

-- | Calculates the variation for a list of numbers.
variance :: Floating a => [a] -> a
-- Explanation for this perhaps non-obvious code:
-- 1. Calculate the mean of the list (avg = average xs)
-- 2. Composite the following: (. operator is function composition)
--   1. Calculate the squared differences from the mean (map ((**2) . (-) avg))
--   2. average that
--   3. take the square root of the average
-- 3. Apply the composite to the list
-- 4. Return the result
variance xs = average . map ((** 2) . (-) avg) $ xs
  where
    !avg = average xs
{-# INLINE variance #-}

-- | Calculate the std. deviation of a list of Floating numbers. Since standard
--   deviation is merely the root of variation, stddev is just a composite of
--   sqrt and variance.
stddev :: Floating a => [a] -> a
-- eta-reduced.
stddev = sqrt . variance
{-# INLINE stddev #-}

-- | Calculate the average of a list of Floating numbers.
average :: Floating a => [a] -> a
-- eta reduced.
average = (/) <$> sum <*> realToFrac . length
{-# INLINE average #-}

-- | Calculate the Covariance of two lists of Floating numbers.
-- they must be of the same length. If they are not, the result is undefined.
covariance :: Floating a => [a] -> [a] -> a
-- You might be looking at this and thinking "WTF is this?". I'll explain.
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
-- realToFrac - convert the length of the list to a fractional number (GHC infers that it is of type a)
-- (/) - divide the sum by the length
-- And that gives us the covariance!
covariance xs ys =
  (/ len)
    . sum
    $ zipWith (*) ((avgX -) <$> xs) ((avgY -) <$> ys)
  where
    !len = realToFrac . length $ xs
    !avgX = average xs
    !avgY = average ys
{-# INLINE covariance #-}

-- | Calculate the Correlation Coefficient of two lists of Floating point numbers.
--    They must be of the same length. If they are not, the result is undefined.
correlation :: Floating a => [a] -> [a] -> a
-- Self explanatory.
correlation xs ys = covariance xs ys / (stddev xs * stddev ys)
{-# INLINE correlation #-}

-- | Calculate the Linear Regression between two Vectors of Floating point numbers.
--      The first Vector is the independent variable, the second is the dependent variable.
--      The result is a tuple of the slope and the y-intercept of the line of best fit.
--      The two Vectors must be of the same length. If they are not, the result is undefined.
linearRegression :: Floating a => [a] -> [a] -> (a, a)
linearRegression xs ys = (alpha, beta)
  where
    !c = covariance xs ys
    !avgx = variance xs
    !avgy = variance ys
    !n = fromIntegral $ length xs
    !beta = c / avgy
    !alpha = avgy - beta / avgx
{-# INLINE linearRegression #-}