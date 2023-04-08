module Data.Series.Generic.Windowing (
    windows,
    iwindows,
    expanding,
    irolling,
) where


import qualified Data.Series.Index      as Index
import           Data.Series.Generic.Definition ( Series(..) )
import qualified Data.Series.Generic.Definition as Series
import           Data.Series.Generic.View       ( Range(..), select, slice )
import           Data.Vector.Generic    ( Vector )
import qualified Data.Vector.Generic    as Vector


windows :: (Vector v a, Ord k) 
        => (k -> Range k) 
        -> Series v k a 
        -> [Series v k a]
windows gen xs = [ xs `select` gen key | key <- Index.toAscList (index xs) ]
{-# INLINE windows #-}


iwindows :: Vector v a => (Int -> Range Int) -> Series v k a -> [Series v k a]
iwindows gen xs = [ slice start (end + 1) xs   -- Recall that `slice` does NOT include the right index
                  | MkRange start end <- gen <$> [0..Series.length xs - 1]
                  ]
{-# INLINE iwindows #-}


expanding :: (Vector v a, Vector v b) 
          => Series v k a        -- ^ Series vector
          -> (Series v k a -> b) -- ^ Aggregation function
          -> Series v k b        -- ^ Resulting vector
expanding vs f = MkSeries (index vs) $ Vector.unfoldrExactN (Series.length vs) go 0
    where
        -- Recall that `slice` does NOT include the right index
        go ix = (f $ slice 0 (ix + 1) vs, ix + 1)
{-# INLINE expanding #-}


-- | Rolling aggregation based on integer indices.
irolling :: (Vector v a, Vector v b)
         => Int               -- ^ Window length
         -> b                 -- ^ Fill value for results which use less than the full window
         -> (Series v k a -> b) -- ^ Aggregation function
         -> Series v k a        -- ^ Input vector
         -> Series v k b        -- ^ Resulting vector
irolling window fillvalue f xs
    | window <= 0 = error $ "Window should be nonnegative, but got " <> show window
    | otherwise 
        = let len = Series.length xs
              go ix 
                | ix - window + 1 < 0 = (fillvalue, ix+1)
                | otherwise       = (f $ slice (ix - window + 1) (ix + 1) xs, ix + 1)
           in MkSeries (index xs) $ Vector.unfoldrExactN len go 0
{-# INLINE irolling #-}


