module Data.Series.Broadcast (
    zipWith, zipWithMatched,
) where

import           Data.Series.Definition ( Series(MkSeries, index) )
import           Data.Series.View       ( select )
import qualified Data.Set               as Set
import qualified Data.Vector            as Vector
import           Prelude                hiding ( zipWith, (<*), (*>), (<*>) ) 

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Set as Set

-- | Apply a function elementwise to two series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value @Nothing@ is returned.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWith (+) xs ys
--   index |  values
--   ----- |  ------
-- "alpha" | Just 10
--  "beta" | Just 12
-- "delta" | Nothing
-- "gamma" | Nothing
--
-- To only combine elements where keys are in both series, see @zipWithMatched@
zipWith :: Ord k => (a -> b -> c) -> Series k a -> Series k b -> Series k (Maybe c)
zipWith f left right
    = let matched = zipWithMatched f left right
          matchedKeys   = index matched
          allKeys       = index left `Set.union` index right
          unmatchedKeys = allKeys `Set.difference` matchedKeys
          unmatched     = MkSeries unmatchedKeys (Vector.replicate (Set.size unmatchedKeys) Nothing)
       in (Just <$> matched) <> unmatched
{-# INLINE zipWith #-}


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithMatched (+) xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
--
-- To combine elements where keys are in either series, see @zipWith@
zipWithMatched :: Ord k => (a -> b -> c) -> Series k a -> Series k b -> Series k c
zipWithMatched f left right
    = let matchedKeys   = index left `Set.intersection` index right

          (MkSeries _ xs) = left  `select` matchedKeys
          (MkSeries _ ys) = right `select` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
          matched         = MkSeries matchedKeys $ Vector.zipWith f xs ys
       in matched
{-# INLINE zipWithMatched #-}
