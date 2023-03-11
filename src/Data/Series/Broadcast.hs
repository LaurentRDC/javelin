module Data.Series.Broadcast (
    zipWith, zipWithMatched,
) where

import           Data.Series.Definition ( Series(MkSeries, index) )
import           Data.Series.View       ( select )
import qualified Data.Set               as Set
import qualified Data.Vector            as Vector
import           Prelude                hiding ( zipWith, (<*), (*>), (<*>) ) 


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value @Nothing@ is returned.
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
zipWithMatched :: Ord k => (a -> b -> c) -> Series k a -> Series k b -> Series k c
zipWithMatched f left right
    = let matchedKeys   = index left `Set.intersection` index right

          (MkSeries _ xs) = left  `select` matchedKeys
          (MkSeries _ ys) = right `select` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
          matched         = MkSeries matchedKeys $ Vector.zipWith f xs ys
       in matched
{-# INLINE zipWithMatched #-}
