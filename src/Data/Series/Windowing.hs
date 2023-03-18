module Data.Series.Windowing (
    windows,
    iwindows,
    expanding,
    irolling,
) where


import qualified Data.Set               as Set               
import           Data.Series.Definition ( Series(..) )
import           Data.Series.View       ( Range(..), select, slice )
import qualified Data.Vector            as Vector


windows :: Ord k 
        => (k -> Range k) 
        -> Series k a 
        -> [Series k a]
windows gen xs = [ xs `select` gen key | key <- Set.toAscList (index xs) ]
{-# INLINE windows #-}


iwindows :: (Int -> Range Int) -> Series k a -> [Series k a]
iwindows gen xs = [ slice start (end + 1) xs   -- Recall that `slice` does NOT include the right index
                  | MkRange start end <- gen <$> [0..length xs - 1]
                  ]
{-# INLINE iwindows #-}


expanding :: Series k a        -- ^ Series vector
          -> (Series k a -> b) -- ^ Aggregation function
          -> Series k b        -- ^ Resulting vector
expanding vs f = MkSeries (index vs) $ Vector.unfoldrExactN (length vs) go 0
    where
        -- Recall that `slice` does NOT include the right index
        go ix = (f $ slice 0 (ix + 1) vs, ix + 1)
{-# INLINE expanding #-}


-- | Rolling aggregation based on integer indices.
irolling :: Int               -- ^ Window length
         -> b                 -- ^ Fill value for results which use less than the full window
         -> (Series k a -> b) -- ^ Aggregation function
         -> Series k a        -- ^ Input vector
         -> Series k b        -- ^ Resulting vector
irolling window fillvalue f xs
    | window <= 0 = error $ "Window should be nonnegative, but got " <> show window
    | otherwise 
        = let len = length xs
              go ix 
                | ix - window + 1 < 0 = (fillvalue, ix+1)
                | otherwise       = (f $ slice (ix - window + 1) (ix + 1) xs, ix + 1)
           in MkSeries (index xs) $ Vector.unfoldrExactN len go 0
{-# INLINE irolling #-}


