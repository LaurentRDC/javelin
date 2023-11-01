-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Series
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
-- This module contains data structures and functions to work with 'Series' capable of holding any Haskell value. 
-- For better performance, at the cost of less flexibility, see the "Data.Series.Unboxed".
--
-- = Introduction to series
--
-- A 'Series' of type @Series k a@ is a labeled array of values of type @a@,
-- indexed by keys of type @k@.
--
-- Like `Data.Map.Strict.Map` from the @containers@ package, 'Series' support efficient:
--
--      * random access by key ( \(O(\log n)\) );
--      * slice by key ( \(O(\log n)\) ).
--
-- Like `Data.Vector.Vector`, they support efficient:
--
--      * random access by index ( \(O(1)\) );
--      * slice by index ( \(O(1)\) );
--      * numerical operations.
--
-- This module re-exports most of the content of "Data.Series.Generic", with type signatures 
-- specialized to the boxed container type `Data.Vector.Vector`.
--
-- For better performance (at the cost of more constraints), especially when it comes to numerical calculations, prefer to
-- use "Data.Series.Unboxed", which contains an implementation of series specialized to the unboxed container type `Data.Vector.Unboxed.Vector`.
 
module Data.Series (
    Series, index, values,

    -- * Building/converting 'Series'
    singleton, fromIndex,
    -- ** Lists
    fromList, toList,
    -- ** Vectors
    fromVector, toVector,
    -- ** Handling duplicates
    Occurrence, fromListDuplicates, fromVectorDuplicates,
    -- ** Strict Maps
    fromStrictMap, toStrictMap,
    -- ** Lazy Maps
    fromLazyMap, toLazyMap,
    -- ** Conversion between 'Series' types
    G.convert,

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, 
    takeWhile, dropWhile, filter,
    -- ** Mapping with effects
    mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_, traverseWithKey,

    -- * Combining series
    zipWith, zipWithMatched, zipWithKey,
    zipWith3, zipWithMatched3, zipWithKey3,
    ZipStrategy, skipStrategy, mapStrategy, constStrategy, zipWithStrategy, zipWithStrategy3,
    zipWithMonoid, esum, eproduct,

    -- * Index manipulation
    require, dropna, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, from, upto, Selection, 
    -- ** Single-element access
    at, iat,
    -- ** Finding indices based on values
    argmax, argmin,

    -- * Replacing values
    replace, (|->), (<-|),

    -- * Scans
    forwardFill,

    -- * Grouping and windowing operations
    groupBy, Grouping, aggregateWith, foldWith, 
    windowing, expanding,

    -- * Numerical aggregation
    mean, var, std, 
    sampleVariance,
    meanAndVariance,
) where

import qualified Data.Map.Lazy       as ML
import qualified Data.Map.Strict     as MS
import           Data.Series.Index   ( Index )
import           Data.Series.Generic ( Range, Selection, ZipStrategy, Occurrence, to, from, upto )
import qualified Data.Series.Generic as G
import           Data.Series.Generic.Zip ( skipStrategy, mapStrategy, constStrategy )
import           Data.Vector         ( Vector )

import           Prelude             hiding (map, zipWith, zipWith3, filter, takeWhile, dropWhile, last)

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Series.Index as Index

infixl 1 `select` 
infix 6 |->, <-|

-- | A series is a labeled array of values of type @a@,
-- indexed by keys of type @k@.
--
-- Like @Data.Map@ and @Data.HashMap@, they support efficient:
--
--      * random access by key ( \(O(\log n)\) );
--      * slice by key ( \(O(\log n)\) ).
--
-- Like @Data.Vector.Vector@, they support efficient:
--
--      * random access by index ( \(O(1)\) );
--      * slice by index ( \(O(1)\) );
--      * numerical operations.
type Series = G.Series Vector


index :: Series k a -> Index k
{-# INLINE index #-}
index = G.index


values :: Series k a -> Vector a
{-# INLINE values #-}
values = G.values


-- | Create a 'Series' with a single element.
singleton :: k -> a -> Series k a
{-# INLINE singleton #-}
singleton = G.singleton


-- | \(O(n)\) Generate a 'Series' by mapping every element of its index.
--
-- >>> fromIndex (const (0::Int)) $ Index.fromList ['a','b','c','d']
-- index | values
-- ----- | ------
--   'a' |      0
--   'b' |      0
--   'c' |      0
--   'd' |      0
fromIndex :: (k -> a) -> Index k -> Series k a
{-# INLINE fromIndex #-}
fromIndex = G.fromIndex


-- | Construct a series from a list of key-value pairs. There is no
-- condition on the order of pairs.
--
-- >>> let xs = fromList [('b', 0::Int), ('a', 5), ('d', 1) ]
-- >>> xs
-- index | values
-- ----- | ------
--   'a' |      5
--   'b' |      0
--   'd' |      1
--
-- If you need to handle duplicate keys, take a look at `fromListDuplicates`.
fromList :: Ord k => [(k, a)] -> Series k a
{-# INLINE fromList #-}
fromList = G.fromList


-- | Construct a series from a list of key-value pairs.
-- Contrary to `fromList`, values at duplicate keys are preserved. To keep each
-- key unique, an `Occurrence` number counts up.
--
-- >>> let xs = fromListDuplicates [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
-- >>> xs
--   index | values
--   ----- | ------
-- ('a',0) |      5
-- ('b',0) |      0
-- ('d',0) |      1
-- ('d',1) |     -4
-- ('d',2) |      7
--
-- Note that due to differences in sorting,
-- 'fromVectorDuplicates' and 'fromListDuplicates' may return results in 
-- a different order.
fromListDuplicates :: Ord k => [(k, a)] -> Series (k, Occurrence) a
{-# INLINE fromListDuplicates #-}
fromListDuplicates = G.fromListDuplicates


-- | Construct a list from key-value pairs. The elements are in order sorted by key:
--
-- >>> let xs = Series.fromList [ ('b', 0::Int), ('a', 5), ('d', 1) ]
-- >>> xs
-- index | values
-- ----- | ------
--   'a' |      5
--   'b' |      0
--   'd' |      1
-- >>> toList xs
-- [('a',5),('b',0),('d',1)]
toList :: Series k a -> [(k, a)]
{-# INLINE toList #-}
toList = G.toList


-- | Construct a 'Vector' of key-value pairs. The elements are in order sorted by key. 
toVector :: Series k a -> Vector (k, a)
{-# INLINE toVector #-}
toVector = G.toVector


-- | Construct a 'Series' from a 'Vector' of key-value pairs. There is no
-- condition on the order of pairs. Duplicate keys are silently dropped. If you
-- need to handle duplicate keys, see 'fromVectorDuplicates'.
--
-- Note that due to differences in sorting,
-- @'Series.fromList'@ and @'Series.fromVector' . 'Vector.fromList'@ 
-- may not be equivalent if the input list contains duplicate keys.
fromVector :: Ord k => Vector (k, a) -> Series k a
{-# INLINE fromVector #-}
fromVector = G.fromVector


-- | Construct a series from a 'Vector' of key-value pairs.
-- Contrary to 'fromVector', values at duplicate keys are preserved. To keep each
-- key unique, an 'Occurrence' number counts up.
--
-- >>> import qualified Data.Vector as Vector
-- >>> let xs = fromVectorDuplicates $ Vector.fromList [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
-- >>> xs
--   index | values
--   ----- | ------
-- ('a',0) |      5
-- ('b',0) |      0
-- ('d',0) |      1
-- ('d',1) |     -4
-- ('d',2) |      7
--
-- Note that due to differences in sorting,
-- 'fromVectorDuplicates' and 'fromListDuplicates' may return results in 
-- a different order.
fromVectorDuplicates :: Ord k => Vector (k, a) -> Series (k, Occurrence) a
{-# INLINE fromVectorDuplicates #-}
fromVectorDuplicates = G.fromVectorDuplicates


-- | Convert a series into a lazy @Map@.
toLazyMap :: Series k a -> ML.Map k a
{-# INLINE toLazyMap #-}
toLazyMap = G.toLazyMap


-- | Construct a series from a lazy @Map@.
fromLazyMap :: ML.Map k a -> Series k a
{-# INLINE fromLazyMap #-}
fromLazyMap = G.fromLazyMap


-- | Convert a series into a strict @Map@.
toStrictMap :: Series k a -> MS.Map k a
{-# INLINE toStrictMap #-}
toStrictMap = G.toStrictMap


-- | Construct a series from a strict @Map@.
fromStrictMap :: MS.Map k a -> Series k a
{-# INLINE fromStrictMap #-}
fromStrictMap = G.fromStrictMap


-- | \(O(n)\) Map every element of a 'Series'.
map :: (a -> b) -> Series k a -> Series k b
{-# INLINE map #-}
map = G.map


-- | \(O(n)\) Map every element of a 'Series', possibly using the key as well.
mapWithKey :: (k -> a -> b) -> Series k a -> Series k b
{-# INLINE mapWithKey #-}
mapWithKey = G.mapWithKey


-- | \(O(n \log n)\).
-- Map each key in the index to another value. Note that the resulting series
-- may have less elements, because each key must be unique.
--
-- In case new keys are conflicting, the first element is kept.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> xs `mapIndex` head
-- index | values
-- ----- | ------
--   'L' |      4
--   'P' |      1
mapIndex :: (Ord k, Ord g) => Series k a -> (k -> g) -> Series g a
{-# INLINE mapIndex #-}
mapIndex = G.mapIndex


-- | /O(n)/ Apply the monadic action to every element of a series and its
-- index, yielding a series of results.
mapWithKeyM :: (Monad m, Ord k) => (k -> a -> m b) -> Series k a -> m (Series k b)
{-# INLINE mapWithKeyM #-}
mapWithKeyM = G.mapWithKeyM


-- | /O(n)/ Apply the monadic action to every element of a series and its
-- index, discarding the results.
mapWithKeyM_ :: Monad m => (k -> a -> m b) -> Series k a -> m ()
{-# INLINE mapWithKeyM_ #-}
mapWithKeyM_ = G.mapWithKeyM_


-- | /O(n)/ Apply the monadic action to all elements of the series and their associated keys, 
-- yielding a series of results.
forWithKeyM :: (Monad m, Ord k) => Series k a -> (k -> a -> m b) -> m (Series k b)
{-# INLINE forWithKeyM #-}
forWithKeyM = G.forWithKeyM


-- | /O(n)/ Apply the monadic action to all elements of the series and their associated keys, 
-- discarding the results.
forWithKeyM_ :: Monad m => Series k a -> (k -> a -> m b) -> m ()
{-# INLINE forWithKeyM_ #-}
forWithKeyM_ = G.forWithKeyM_


-- | /O(n)/ Traverse a 'Series' with an Applicative action, taking into account both keys and values. 
traverseWithKey :: (Applicative t, Ord k)
                => (k -> a -> t b) 
                -> Series k a 
                -> t (Series k b)
{-# INLINE traverseWithKey #-}
traverseWithKey = G.traverseWithKey


-- | \(O(n)\) Returns the longest prefix (possibly empty) of the input 'Series' that satisfy a predicate.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4), ("Vienna", 5)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- "Vienna" |      5

-- >>> takeWhile (>1) xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
takeWhile :: (a -> Bool) -> Series k a -> Series k a
takeWhile = G.takeWhile


-- | \(O(n)\) Returns the complement of `takeWhile`.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4), ("Vienna", 5)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- "Vienna" |      5

-- >>> dropWhile (>1) xs
--    index | values
--    ----- | ------
--  "Paris" |      1
-- "Vienna" |      5
dropWhile :: (a -> Bool) -> Series k a -> Series k a
dropWhile = G.dropWhile


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value 'Nothing' is returned.
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
-- To only combine elements where keys are in both series, see 'zipWithMatched'.
zipWith :: (Ord k) 
        => (a -> b -> c) -> Series k a -> Series k b -> Series k (Maybe c)
zipWith = G.zipWith 
{-# INLINE zipWith #-}



-- | Apply a function elementwise to three series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value 'Nothing' is returned.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int),  ("beta", 1),   ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11),  ("delta", 13) ]
-- >>> let zs = Series.fromList [ ("alpha", 20::Int), ("delta", 13), ("epsilon", 6) ]
-- >>> zipWith3 (\x y z -> x + y + z) xs ys zs
--     index |  values
--     ----- |  ------
--   "alpha" | Just 30
--    "beta" | Nothing
--   "delta" | Nothing
-- "epsilon" | Nothing
--   "gamma" | Nothing
--
-- To only combine elements where keys are in all series, see 'zipWithMatched3'
zipWith3 :: (Ord k) 
         => (a -> b -> c -> d) 
         -> Series k a 
         -> Series k b 
         -> Series k c 
         -> Series k (Maybe d)
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3


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
-- To combine elements where keys are in either series, see 'zipWith'.
zipWithMatched :: Ord k => (a -> b -> c) -> Series k a -> Series k b -> Series k c
{-# INLINE zipWithMatched #-}
zipWithMatched = G.zipWithMatched


-- | Apply a function elementwise to three series, matching elements
-- based on their keys. Keys not present in all three series are dropped.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int),  ("beta", 1),   ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11),  ("delta", 13) ]
-- >>> let zs = Series.fromList [ ("alpha", 20::Int), ("delta", 13), ("epsilon", 6) ]
-- >>> zipWithMatched3 (\x y z -> x + y + z) xs ys zs
--   index | values
--   ----- | ------
-- "alpha" |     30
zipWithMatched3 :: (Ord k) 
                => (a -> b -> c -> d) 
                -> Series k a 
                -> Series k b 
                -> Series k c
                -> Series k d
{-# INLINE zipWithMatched3 #-}
zipWithMatched3 = G.zipWithMatched3


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
-- 
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithKey (\k x y -> length k + x + y) xs ys
--   index | values
--   ----- | ------
-- "alpha" |     15
--  "beta" |     16
--
-- To combine elements where keys are in either series, see 'zipWith'
zipWithKey :: (Ord k) 
           => (k -> a -> b -> c) -> Series k a -> Series k b -> Series k c
{-# INLINE zipWithKey #-}
zipWithKey = G.zipWithKey


-- | Apply a function elementwise to three series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
-- 
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> let zs = Series.fromList [ ("alpha", 20::Int), ("beta", 7), ("delta", 5) ]
-- >>> zipWithKey3 (\k x y z -> length k + x + y + z) xs ys zs
--   index | values
--   ----- | ------
-- "alpha" |     35
--  "beta" |     23
--
-- To combine elements where keys are in either series, see 'zipWith'
zipWithKey3 :: (Ord k) 
            => (k -> a -> b -> c -> d) 
            -> Series k a 
            -> Series k b 
            -> Series k c
            -> Series k d
{-# INLINE zipWithKey3 #-}
zipWithKey3 = G.zipWithKey3


-- | Zip two 'Series' with a combining function, applying a `ZipStrategy` when one key is present in one of the 'Series' but not both.
--
-- In the example below, we want to set the value to @-100@ (via @`constStrategy` (-100)@) for keys which are only present 
-- in the left 'Series', and drop keys (via `skipStrategy`) which are only present in the `right 'Series'  
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithStrategy (+) (constStrategy (-100)) skipStrategy  xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
-- "gamma" |   -100
--
-- Note that if you want to drop keys missing in either 'Series', it is faster to use @`zipWithMatched` f@ 
-- than using @`zipWithStrategy` f skipStrategy skipStrategy@.
zipWithStrategy :: (Ord k) 
               => (a -> b -> c)     -- ^ Function to combine values when present in both series
               -> ZipStrategy k a c -- ^ Strategy for when the key is in the left series but not the right
               -> ZipStrategy k b c -- ^ Strategy for when the key is in the right series but not the left
               -> Series k a
               -> Series k b 
               -> Series k c
{-# INLINE zipWithStrategy #-}
zipWithStrategy = G.zipWithStrategy


-- | Zip three 'Series' with a combining function, applying a 'ZipStrategy' when one key is 
-- present in one of the 'Series' but not all of the others.
--
-- Note that if you want to drop keys missing in either 'Series', it is faster to use @'zipWithMatched3' f@ 
-- than using @'zipWithStrategy3' f skipStrategy skipStrategy skipStrategy@.
zipWithStrategy3 :: (Ord k) 
                => (a -> b -> c -> d) -- ^ Function to combine values when present in all series
                -> ZipStrategy k a d  -- ^ Strategy for when the key is in the left series but not in all the others
                -> ZipStrategy k b d  -- ^ Strategy for when the key is in the center series but not in all the others
                -> ZipStrategy k c d  -- ^ Strategy for when the key is in the right series but not in all the others
                -> Series k a
                -> Series k b 
                -> Series k c
                -> Series k d
{-# INLINE zipWithStrategy3 #-}
zipWithStrategy3 = G.zipWithStrategy3


-- | Zip two 'Series' with a combining function. The value for keys which are missing from
-- either 'Series' is replaced with the appropriate `mempty` value.
--
-- >>> import Data.Monoid ( Sum(..) )
-- >>> let xs = Series.fromList [ ("2023-01-01", Sum (1::Int)), ("2023-01-02", Sum 2) ]
-- >>> let ys = Series.fromList [ ("2023-01-01", Sum (5::Int)), ("2023-01-03", Sum 7) ]
-- >>> Series.zipWith (<>) xs ys
--        index |                  values
--        ----- |                  ------
-- "2023-01-01" | Just (Sum {getSum = 6})
-- "2023-01-02" |                 Nothing
-- "2023-01-03" |                 Nothing
-- >>> zipWithMonoid (<>) xs ys
--        index |           values
--        ----- |           ------
-- "2023-01-01" | Sum {getSum = 6}
-- "2023-01-02" | Sum {getSum = 2}
-- "2023-01-03" | Sum {getSum = 7}
zipWithMonoid :: ( Monoid a, Monoid b, Ord k) 
              => (a -> b -> c)
              -> Series k a
              -> Series k b 
              -> Series k c
zipWithMonoid = G.zipWithMonoid
{-# INLINE zipWithMonoid #-}


-- | Elementwise sum of two 'Series'. Elements missing in one or the other 'Series' is considered 0. 
--
-- >>> let xs = Series.fromList [ ("2023-01-01", (1::Int)), ("2023-01-02", 2) ]
-- >>> let ys = Series.fromList [ ("2023-01-01", (5::Int)), ("2023-01-03", 7) ]
-- >>> xs `esum` ys
--        index | values
--        ----- | ------
-- "2023-01-01" |      6
-- "2023-01-02" |      2
-- "2023-01-03" |      7
esum :: (Ord k, Num a) 
     => Series k a 
     -> Series k a
     -> Series k a
esum = G.esum
{-# INLINE esum #-}


-- | Elementwise product of two 'Series'. Elements missing in one or the other 'Series' is considered 1. 
--
-- >>> let xs = Series.fromList [ ("2023-01-01", (2::Int)), ("2023-01-02", 3) ]
-- >>> let ys = Series.fromList [ ("2023-01-01", (5::Int)), ("2023-01-03", 7) ]
-- >>> xs `eproduct` ys
--        index | values
--        ----- | ------
-- "2023-01-01" |     10
-- "2023-01-02" |      3
-- "2023-01-03" |      7
eproduct :: (Ord k, Num a) 
         => Series k a 
         -> Series k a
         -> Series k a
eproduct = G.eproduct
{-# INLINE eproduct #-}


-- | Require a series to have a specific `Index`.
-- Contrary to @select@, all keys in the `Index` will be present in the resulting series.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> xs `require` Index.fromList ["Paris", "Lisbon", "Taipei"]
--    index |  values
--    ----- |  ------
-- "Lisbon" |  Just 4
--  "Paris" |  Just 1
-- "Taipei" | Nothing
require :: Ord k => Series k a -> Index k -> Series k (Maybe a)
{-# INLINE require #-}
require = G.require 


-- | Drop the index of a series by replacing it with an `Int`-based index. Values will
-- be indexed from 0.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> dropIndex xs
-- index | values
-- ----- | ------
--     0 |      4
--     1 |      2
--     2 |      1
dropIndex :: Series k a -> Series Int a
{-# INLINE dropIndex #-}
dropIndex = G.dropIndex


-- | Filter elements. Only elements for which the predicate is @True@ are kept. 
-- Notice that the filtering is done on the values, not on the keys
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> filter (>2) xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
filter :: Ord k => (a -> Bool) -> Series k a -> Series k a
{-# INLINE filter #-}
filter = G.filter


-- | Drop elements which are not available (NA). 
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> let ys = xs `require` Index.fromList ["Paris", "London", "Lisbon", "Toronto"]
-- >>> ys
--     index |  values
--     ----- |  ------
--  "Lisbon" |  Just 4
--  "London" |  Just 2
--   "Paris" |  Just 1
-- "Toronto" | Nothing
-- >>> dropna ys
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
dropna :: Ord k => Series k (Maybe a) -> Series k a
{-# INLINE dropna #-}
dropna = G.dropna


-- | Select a subseries. There are a few ways to do this.
--
-- The first way to do this is to select a sub-series based on random keys. For example,
-- selecting a subseries from an `Index`:
--
-- >>> let xs = Series.fromList [('a', 10::Int), ('b', 20), ('c', 30), ('d', 40)]
-- >>> xs `select` Index.fromList ['a', 'd']
-- index | values
-- ----- | ------
--   'a' |     10
--   'd' |     40
--
-- The second way to select a sub-series is to select all keys in a range:
--
-- >>> xs `select` 'b' `to` 'c'
-- index | values
-- ----- | ------
--   'b' |     20
--   'c' |     30
--
-- Note that with `select`, you'll always get a sub-series; if you ask for a key which is not
-- in the series, it'll be ignored:
--
-- >>> xs `select` Index.fromList ['a', 'd', 'e']
-- index | values
-- ----- | ------
--   'a' |     10
--   'd' |     40
--
-- See `require` if you want to ensure that all keys are present.
select :: (Selection s, Ord k) => Series k a -> s k -> Series k a
select = G.select


-- | Select a sub-series from a series matching a condition.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> xs `selectWhere` (fmap (>1) xs)
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
selectWhere :: Ord k => Series k a -> Series k Bool -> Series k a
{-# INLINE selectWhere #-}
selectWhere = G.selectWhere


-- | \(O(\log n)\). Extract a single value from a series, by key.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs `at` "Paris"
-- Just 1
-- >>> xs `at` "Sydney"
-- Nothing
at :: Ord k => Series k a -> k -> Maybe a
{-# INLINE at #-}
at = G.at


-- | \(O(1)\). Extract a single value from a series, by index.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> xs `iat` 0
-- Just 4
-- >>> xs `iat` 3
-- Nothing
iat :: Series k a -> Int -> Maybe a
{-# INLINE iat #-}
iat = G.iat


-- | \(O(n)\) Find the index of the maximum element in the input series.
-- If the input series is empty, 'Nothing' is returned.
--
-- The index of the first occurrence of the maximum element is returned.
--
-- >>> :{ 
--     let (xs :: Series Int Int) 
--          = Series.fromList [ (1, 0)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 7)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in argmax xs 
-- :}
-- Just 4
argmax :: Ord a => Series k a -> Maybe k
argmax = G.argmax


-- | \(O(n)\) Find the index of the minimum element in the input series.
-- If the input series is empty, 'Nothing' is returned.
--
-- The index of the first occurrence of the minimum element is returned.
-- >>> :{ 
--     let (xs :: Series Int Int) 
--          = Series.fromList [ (1, 1)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 0)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in argmin xs 
-- :}
-- Just 4
argmin :: Ord a => Series k a -> Maybe k
argmin = G.argmin


-- | Replace values in the right series from values in the left series at matching keys.
-- Keys not in the right series are unaffected.
-- 
-- See `(|->)` and `(<-|)`, which might be more readable.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> let ys = Series.singleton "Paris" (99::Int)
-- >>> ys `replace` xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |     99
replace :: Ord k => Series k a -> Series k a -> Series k a
{-# INLINE replace #-}
replace = G.replace


-- | Replace values in the right series from values in the left series at matching keys.
-- Keys not in the right series are unaffected.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> let ys = Series.singleton "Paris" (99::Int)
-- >>> ys |-> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |     99
(|->) :: (Ord k) => Series k a -> Series k a -> Series k a
{-# INLINE (|->) #-}
(|->) = (G.|->)


-- | Replace values in the left series from values in the right series at matching keys.
-- Keys not in the left series are unaffected.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> let ys = Series.singleton "Paris" (99::Int)
-- >>> xs <-| ys
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |     99
(<-|) :: (Ord k) => Series k a -> Series k a -> Series k a
{-# INLINE (<-|) #-}
(<-|) = (G.<-|)


-- | /O(n)/ Replace all instances of 'Nothing' with the last previous
-- value which was not 'Nothing'.
--
-- >>> let xs = Series.fromList (zip [0..] [Just 1, Just 2,Nothing, Just 3]) :: Series Int (Maybe Int)
-- >>> xs
-- index |  values
-- ----- |  ------
--     0 |  Just 1
--     1 |  Just 2
--     2 | Nothing
--     3 |  Just 3
-- >>> forwardFill 0 xs
-- index | values
-- ----- | ------
--     0 |      1
--     1 |      2
--     2 |      2
--     3 |      3
--
-- If the first entry of the series is missing, the first input to 'forwardFill' will be used:
--
-- >>> let ys = Series.fromList (zip [0..] [Nothing, Just 2,Nothing, Just 3]) :: Series Int (Maybe Int)
-- >>> ys
-- index |  values
-- ----- |  ------
--     0 | Nothing
--     1 |  Just 2
--     2 | Nothing
--     3 |  Just 3
-- >>> forwardFill 0 ys
-- index | values
-- ----- | ------
--     0 |      0
--     1 |      2
--     2 |      2
--     3 |      3
forwardFill :: a -- ^ Until the first non-'Nothing' is found, 'Nothing' will be filled with this value.
            -> Series v (Maybe a)
            -> Series v a
{-# INLINE forwardFill #-}
forwardFill = G.forwardFill


-- | Group values in a 'Series' by some grouping function (@k -> g@).
-- The provided grouping function is guaranteed to operate on a non-empty 'Series'.
--
-- This function is expected to be used in conjunction with @aggregate@:
-- 
-- >>> type Date = (Int, String)
-- >>> month :: (Date -> String) = snd
-- >>> :{ 
--     let xs = Series.fromList [ ((2020, "January") :: Date,  0 :: Int)
--                              , ((2021, "January"), -5)
--                              , ((2020, "June")   , 20)
--                              , ((2021, "June")   , 25) 
--                              ]
--      in xs `groupBy` month `aggregateWith` minimum
-- :}
--     index | values
--     ----- | ------
-- "January" |     -5
--    "June" |     20
groupBy :: Series k a      -- ^ Grouping function
        ->(k -> g)         -- ^ Input series
        -> Grouping k g a  -- ^ Grouped series
{-# INLINE groupBy #-}
groupBy = G.groupBy

-- | Representation of a 'Series' being grouped.
type Grouping k g a = G.Grouping k g Vector a


-- | Aggregate groups resulting from a call to 'groupBy':
-- 
-- >>> type Date = (Int, String)
-- >>> month :: (Date -> String) = snd
-- >>> :{ 
--     let xs = Series.fromList [ ((2020, "January") :: Date,  0 :: Int)
--                              , ((2021, "January"), -5)
--                              , ((2020, "June")   , 20)
--                              , ((2021, "June")   , 25) 
--                              ]
--      in xs `groupBy` month `aggregateWith` minimum
-- :}
--     index | values
--     ----- | ------
-- "January" |     -5
--    "June" |     20
--
-- If you want to aggregate groups using a binary function, see 'foldWith' which
-- may be much faster.
aggregateWith :: (Ord k, Ord g) 
              => Grouping k g a 
              -> (Series k a -> b) 
              -> Series g b
{-# INLINE aggregateWith #-}
aggregateWith = G.aggregateWith


-- | Aggregate each group in a 'Grouping' using a binary function.
-- While this is not as expressive as 'aggregate', users looking for maximum
-- performance should use 'foldWith' as much as possible.
foldWith :: Ord g 
         => Grouping k g a
         -> (a -> a -> a)
         -> Series g a
{-# INLINE foldWith #-}
foldWith = G.foldWith


-- | Expanding window aggregation.
--
-- >>> import qualified Data.Series as Series 
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
--          = Series.fromList [ (1, 0)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 3)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in (xs `expanding` sum) :: Series.Series Int Int 
-- :}
-- index | values
-- ----- | ------
--     1 |      0
--     2 |      1
--     3 |      3
--     4 |      6
--     5 |     10
--     6 |     15
expanding :: Series k a        -- ^ Series vector
          -> (Series k a -> b) -- ^ Aggregation function
          -> Series k b        -- ^ Resulting vector
{-# INLINE expanding #-}
expanding = G.expanding


-- | General-purpose window aggregation.
--
-- >>> import qualified Data.Series as Series 
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
--          = Series.fromList [ (1, 0)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 3)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in windowing (\k -> k `to` (k+2)) sum xs
-- :}
-- index | values
-- ----- | ------
--     1 |      3
--     2 |      6
--     3 |      9
--     4 |     12
--     5 |      9
--     6 |      5
windowing :: Ord k
          => (k -> Range k)
          -> (Series k a -> b)
          -> Series k a
          -> Series k b
{-# INLINE windowing #-}
windowing = G.windowing


-- | Compute the mean of the values in the series.
-- An empty series will have a mean of NaN.
mean :: (Real a, RealFloat b) => Series k a -> b
{-# INLINE mean #-}
mean = G.mean


-- | Compute the mean and variance of the values in a series in a single-pass.
meanAndVariance :: (RealFloat a) => Series k a -> (a, a)
{-# INLINE meanAndVariance #-}
meanAndVariance = G.meanAndVariance


-- | Population variance.
var :: (RealFloat a) => Series k a -> a
{-# INLINE var #-}
var = G.var


-- | Population standard deviation.
std :: (RealFloat a) => Series k a -> a
{-# INLINE std #-}
std = sqrt . var


-- | Sample variance.
sampleVariance :: (RealFloat a) => Series k a -> a
{-# INLINE sampleVariance #-}
sampleVariance = G.sampleVariance
