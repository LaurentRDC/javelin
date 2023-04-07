module Data.Series.Broadcast (
    zipWith, zipWithMatched,
    -- * Generalized zipping with strategies
    zipWithStrategy,
    ZipStrategy,
    dropStrategy,
    constStrategy,
) where

import           Data.Series.Definition ( Series(MkSeries, index), mapWithKey )
import qualified Data.Series.Index      as Index
import           Data.Series.View       ( select, dropna )
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
          allKeys       = index left `Index.union` index right
          unmatchedKeys = allKeys `Index.difference` matchedKeys
          unmatched     = MkSeries unmatchedKeys (Vector.replicate (Index.size unmatchedKeys) Nothing)
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
    = let matchedKeys   = index left `Index.intersection` index right

          (MkSeries _ xs) = left  `select` matchedKeys
          (MkSeries _ ys) = right `select` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
          matched         = MkSeries matchedKeys $ Vector.zipWith f xs ys
       in matched
{-# INLINE zipWithMatched #-}


-- | A `ZipStrategy` is a function which is used to decide what to do when a key is missing from one
-- of two `Series` being zipped together.
-- A `ZipStrategy` is expected to be used in conjunction with the `zipWithStrategy` function.  
type ZipStrategy k a b = (k -> a -> Maybe b)

-- | This `ZipStrategy` drops keys which are not present in both `Series`.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithStrategy (+) dropStrategy dropStrategy xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
dropStrategy :: ZipStrategy k a b
dropStrategy _ _ = Nothing
{-# INLINE dropStrategy #-}

-- | This `ZipStrategy` sets a constant value at keys which are not present in both `Series`.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithStrategy (+) (constStrategy (-100)) (constStrategy 200)  xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
-- "delta" |    200
-- "gamma" |   -100
constStrategy :: b -> ZipStrategy k a b
constStrategy v = \_ _ -> Just v
{-# INLINE constStrategy #-}


-- | Zip two `Series` with a combining function, applying a `ZipStrategy` when one key is present in one of the `Series` but not both.
--
-- In the example below, we want to set the value to @-100@ (via @`constStrategy` (-100)@) for keys which are only present 
-- in the left `Series`, and drop keys (via `dropStrategy`) which are only present in the `right `Series`  
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithStrategy (+) (constStrategy (-100)) dropStrategy  xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
-- "gamma" |   -100
--
-- Note that if you want to drop keys missing in either `Series`, it is faster to use @`zipWithMatched` f@ 
-- than using @`zipWithStrategy` f dropStrategy dropStrategy@.
zipWithStrategy :: Ord k 
               => (a -> b -> c)     -- ^ Function to combine values when present in both series
               -> ZipStrategy k a c -- ^ Strategy for when the key is in the left series but not the right
               -> ZipStrategy k b c -- ^ Strategy for when the key is in the right series but not the left
               -> Series k a
               -> Series k b 
               -> Series k c
zipWithStrategy f whenLeft whenRight left right 
    = let matchedKeys   = index left  `Index.intersection` index right

          (MkSeries _ xs) = left  `select` matchedKeys
          (MkSeries _ ys) = right `select` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
          matched         = MkSeries matchedKeys $ Vector.zipWith f xs ys

          onlyLeftKeys  = index left  `Index.difference` index right
          onlyRightKeys = index right `Index.difference` index left

          leftZip =  dropna $ mapWithKey whenLeft  $ left  `select` onlyLeftKeys
          rightZip = dropna $ mapWithKey whenRight $ right `select` onlyRightKeys
          
        in matched <> leftZip <> rightZip
{-# INLINE zipWithStrategy #-}