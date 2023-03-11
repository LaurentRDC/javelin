module Data.Series.Numeric ( 
    mean, variance, sampleVariance,
    meanAndVariance,
    std, 
    -- * References
    -- $references
) where


import           Data.Series.Definition ( Series(..) )
import qualified Data.Vector            as Vector


-- | Compute the mean of the values in the series.
-- An empty series will have a mean of NaN.
mean :: (Real a, RealFloat b) => Series k a -> b
mean xs = realToFrac (sum xs) 
        / fromIntegral (length xs)
{-# INLINE mean #-}


-- | Online Welford algorithm, which computes the mean and variance in a single pass.

-- Returns:
--  * The number of elements in the series;
--  * The average of the series;
--  * The squared average of the series. 
welford :: (Real a, Real b, Floating b) => Series k a -> (Int, b, b)
welford (MkSeries _ xs)
    | null xs   = (0, 0, 0)
    | otherwise = Vector.last 
                $ Vector.scanl (\(c, m, m2) e -> let delta = realToFrac e - m
                                                     newMean = m + delta / fromIntegral newCount 
                                                     newCount = c + 1
                                                  in ( newCount
                                                     , newMean
                                                     , m2 + delta * (realToFrac e - newMean)
                                                     )
                               )
                               (0 :: Int, 0, 0)
                               xs
{-# INLINE welford #-}


-- | Compute the mean and variance of the values in a series in a single-pass.
meanAndVariance :: (Real a, Real b, Floating b) => Series k a -> (b, b)
meanAndVariance xs 
    | null xs   = (0/0, 0/0) -- The mean computed by `welford` will be 0 if no elements.
    | otherwise = let (count, m, meanSquared) = welford xs
                   in (m, meanSquared / fromIntegral count)


-- | Population variance.
variance :: (Real a, Real b, Floating b) => Series k a -> b
variance xs = let (count, _, meanSquared) = welford xs
               in meanSquared / fromIntegral count
{-# INLINE variance #-}


-- | Population standard deviation.
std :: (Real a, Real b, Floating b) => Series k a -> b
std = sqrt . variance
{-# INLINE std #-}


-- | Sample variance.
sampleVariance :: (Real a, Real b, Floating b) => Series k a -> b
sampleVariance xs
    | length xs < 2 = 0/0
    | otherwise = let (count, _, meanSquared) = welford xs
                   in meanSquared / fromIntegral (count - 1)
{-# INLINE sampleVariance #-}

-- $references
--
-- * West, D.H.D., Updating mean and variance estimates: an improved method (1979). 
--   /Communications of the ACM/ <http://doi.acm.org/10.1145/359146.359153>