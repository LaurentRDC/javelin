module Data.Series.Numeric ( 
    mean, nanmean,
    var, nanvar, 
    sampleVariance, nanSampleVariance,
    meanAndVariance, nanMeanAndVariance,
    std, nanstd
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


-- Ensure that an aggregation function is 0 when a series is empty
zeroIfEmpty :: Num a => (Series k a -> a) -> (Series k a -> a)
zeroIfEmpty f xs = if null xs then 0 else f xs
{-# INLINE zeroIfEmpty #-}


-- | Sum all the values in the series, ignoring values of NaN.
-- An empty series will have a sum of 0.
nansum :: (RealFloat a) => Series k a -> a
nansum = Vector.sum . Vector.filter (not . isNaN) . values
{-# INLINE nansum #-}


-- | Compute the mean of the values in the series, ignoring NaN entries.
-- An empty series will have a mean of 0.
nanmean :: RealFloat a => Series k a -> a
nanmean = zeroIfEmpty $ \xs -> nansum xs / fromIntegral (length xs)
{-# INLINE nanmean #-}


-- | Online Welford algorithm, which computes the mean and variance in a single pass.

-- Returns:
--  * The number of elements in the series;
--  * The average of the series;
--  * The squared average of the series. 
welford :: RealFloat a 
        => (a -> Bool)  -- Filtering predicate, most commonly used to ignore NaN
        -> Series k a 
        -> (Int, a, a)
welford predicate (MkSeries _ xs)
    | null xs   = (0, 0, 0)
    | otherwise = Vector.last 
                $ Vector.scanl (\(c, m, m2) e -> let delta = e - m
                                                     newMean = m + delta / fromIntegral newCount 
                                                     newCount = c + 1
                                                  in ( newCount
                                                     , newMean
                                                     , m2 + delta * (e - newMean)
                                                     )
                               )
                               (0 :: Int, 0, 0)
                               $ Vector.filter predicate xs
{-# INLINE welford #-}


-- | Compute the mean and variance of the values in a series in a single-pass.
meanAndVariance :: RealFloat a => Series k a -> (a, a)
meanAndVariance xs 
    | null xs   = (0/0, 0/0) -- The mean computed by `welford` will be 0 if no elements.
    | otherwise = let (count, m, meanSquared) = welford (const True) xs
                   in (m, meanSquared / fromIntegral count)


-- | Compute the mean and variance of the values in a series in a single-pass, ignoring NaN.
-- Returns @(0, 0)@ for empty series.
nanMeanAndVariance :: RealFloat a => Series k a -> (a, a)
nanMeanAndVariance xs 
    | null xs   = (0, 0)
    | otherwise = let (count, m, meanSquared) = welford (not . isNaN) xs
                   in (m, meanSquared / fromIntegral count)


-- | Population variance.
var ::  RealFloat a => Series k a -> a
var xs = let (count, _, meanSquared) = welford (const True) xs
               in meanSquared / fromIntegral count
{-# INLINE var #-}


-- | Population standard deviation.
std ::  RealFloat a => Series k a -> a
std = sqrt . var
{-# INLINE std #-}


-- | Sample variance.
sampleVariance ::  RealFloat a => Series k a -> a
sampleVariance xs
    | length xs < 2 = 0/0
    | otherwise = let (count, _, meanSquared) = welford (const True) xs
                   in meanSquared / fromIntegral (count - 1)
{-# INLINE sampleVariance #-}


-- | Population variance, ignoring NaN entries. Returns 0 for empty series.
nanvar ::  RealFloat a => Series k a -> a
nanvar = zeroIfEmpty $ \xs -> let (count, _, meanSquared) = welford (not . isNaN) xs
                                    in meanSquared / fromIntegral count
{-# INLINE nanvar #-}


-- | Population standard deviation, ignoring NaN entries. Returns 0 for empty series.
nanstd ::  RealFloat a => Series k a -> a
nanstd = sqrt . nanvar
{-# INLINE nanstd #-}


-- | Sample variance, ignoring NaN entries. Returns 0 for empty series.
nanSampleVariance ::  RealFloat a => Series k a -> a
nanSampleVariance xs
    | length xs < 2 = 0
    | otherwise     = let (count, _, meanSquared) = welford (not . isNaN) xs
                       in meanSquared / fromIntegral (count - 1)
{-# INLINE nanSampleVariance #-}

-- $references
--
-- * West, D.H.D., Updating mean and variance estimates: an improved method (1979). 
--   /Communications of the ACM/ <http://doi.acm.org/10.1145/359146.359153>