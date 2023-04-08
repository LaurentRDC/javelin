module Data.Series.Generic.Broadcast (
    zipWith, zipWithMatched,
    -- * Generalized zipping with strategies
    zipWithStrategy,
    ZipStrategy,
    skipStrategy,
    constStrategy,
) where

import           Data.Series.Generic.Definition ( Series(MkSeries, index), mapWithKey )
import qualified Data.Series.Generic.Definition as G
import           Data.Series.Generic.View       ( select, dropna )
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import qualified Data.Series.Index              as Index
import           Prelude                        hiding ( zipWith ) 

-- $setup
-- >>> import qualified Data.Series as Series

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
zipWith :: (Vector v a, Vector v b, Vector v c, Vector v (Maybe c), Ord k) 
        => (a -> b -> c) -> Series v k a -> Series v k b -> Series v k (Maybe c)
zipWith f left right
    = let matched = zipWithMatched f left right
          matchedKeys   = index matched
          allKeys       = index left `Index.union` index right
          unmatchedKeys = allKeys `Index.difference` matchedKeys
          unmatched     = MkSeries unmatchedKeys (Vector.replicate (Index.size unmatchedKeys) Nothing)
       in (G.map Just matched) <> unmatched
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
zipWithMatched :: (Vector v a, Vector v b, Vector v c, Ord k) 
               => (a -> b -> c) -> Series v k a -> Series v k b -> Series v k c
zipWithMatched f left right
    = let matchedKeys   = index left `Index.intersection` index right

          (MkSeries _ xs) = left  `select` matchedKeys
          (MkSeries _ ys) = right `select` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
          matched         = MkSeries matchedKeys $ Vector.zipWith f xs ys
       in matched
{-# INLINE zipWithMatched #-}


-- | A `ZipStrategy` is a function which is used to decide what to do when a key is missing from one
-- of two `Series` being zipped together with `zipWithStrategy`.
--
-- If a `ZipStrategy` returns @Nothing@, the key is dropped.
-- If a `ZipStrategy` returns @Just v@ for key @k@, then the value @v@ is inserted at key @k@.
--
-- For example, the most basic `ZipStrategy` is to skip over any key which is missing from the other series.
-- Such a strategy can be written as @skip key value = Nothing@ (see `skipStrategy`).
type ZipStrategy k a b = (k -> a -> Maybe b)

-- | This `ZipStrategy` drops keys which are not present in both `Series`.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithStrategy (+) skipStrategy skipStrategy xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
skipStrategy :: ZipStrategy k a b
skipStrategy _ _ = Nothing
{-# INLINE skipStrategy #-}


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
-- in the left `Series`, and drop keys (via `skipStrategy`) which are only present in the `right `Series`.  
--
-- Note that if you want to drop keys missing in either `Series`, it is faster to use @`zipWithMatched` f@ 
-- than using @`zipWithStrategy` f skipStrategy skipStrategy@.
zipWithStrategy :: (Vector v a, Vector v b, Vector v c, Vector v (Maybe c), Vector v Int, Ord k) 
               => (a -> b -> c)     -- ^ Function to combine values when present in both series
               -> ZipStrategy k a c -- ^ Strategy for when the key is in the left series but not the right
               -> ZipStrategy k b c -- ^ Strategy for when the key is in the right series but not the left
               -> Series v k a
               -> Series v k b 
               -> Series v k c
zipWithStrategy f whenLeft whenRight left right 
    = let onlyLeftKeys  = index left  `Index.difference` index right
          onlyRightKeys = index right `Index.difference` index left

          leftZip =  dropna $ mapWithKey whenLeft  $ left  `select` onlyLeftKeys
          rightZip = dropna $ mapWithKey whenRight $ right `select` onlyRightKeys
          
        in zipWithMatched f left right <> leftZip <> rightZip
{-# INLINE zipWithStrategy #-}