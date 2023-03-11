module Data.Series.Numeric ( 
    mean, variance, sampleVariance,
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