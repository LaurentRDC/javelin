-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Series.Unboxed
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
-- This module contains data structures and functions to work with 'Series' capable of holding unboxed values,
-- i.e. values of types which are instances of `Unbox`.
--
-- = Why use unboxed series?
--
-- Unboxed series can have much better performance, at the cost of less flexibility. For example,
-- an unboxed series cannot contain values of type @`Maybe` a@. Moreover, unboxed series aren't instances of 
-- `Functor` or `Foldable`.
--
-- If you are hesitating, you should prefer the series implementation in the "Data.Series" module.
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
 
module Data.Series.Unboxed (
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
    map, mapWithKey, mapIndex, null, length,
    takeWhile, dropWhile, filter, filterWithKey,
    -- ** Mapping with effects
    mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_,

    -- * Combining series
    zipWithMatched, zipWithKey,
    zipWithMatched3, zipWithKey3,
    ZipStrategy, skipStrategy, mapStrategy, constStrategy, zipWithStrategy, zipWithStrategy3,
    zipWithMonoid, esum, eproduct, unzip, unzip3,

    -- * Index manipulation
    require, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, from, upto, Selection, 
    -- ** Single-element access
    at, iat,
    -- ** Finding indices based on values
    argmax, argmin,

    -- * Replacement
    replace, (|->), (<-|),

    -- * Grouping and windowing operations
    groupBy, Grouping, aggregateWith, foldWith, 
    windowing, expanding,

    -- * Folds
    -- ** General folds
    foldMap, foldMap', 
    -- ** Specialized folds
    all, any, and, or, sum, product, maximum, minimum,

    -- * aggregation
    mean, var, std, 
    sampleVariance,
    meanAndVariance,
) where

import qualified Data.Map.Lazy       as ML
import qualified Data.Map.Strict     as MS
import           Data.Series.Index   ( Index )
import           Data.Series.Generic.View 
                                     ( Range, Selection, to, from, upto )
import           Data.Series.Generic ( ZipStrategy, Occurrence, skipStrategy, mapStrategy, constStrategy )
import qualified Data.Series.Generic as G
import           Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as Vector

import           Prelude             hiding ( map, zipWith, filter, foldMap, all, any, and, or
                                            , sum, product, maximum, minimum, takeWhile, dropWhile
                                            , last, unzip, unzip3
                                            )

-- $setup
-- >>> import qualified Data.Series.Unboxed as Series
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
singleton :: Unbox a => k -> a -> Series k a
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
fromIndex :: Unbox a
          => (k -> a) -> Index k -> Series k a
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
fromList :: (Unbox a, Ord k) => [(k, a)] -> Series k a
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
fromListDuplicates :: (Unbox a, Ord k) => [(k, a)] -> Series (k, Occurrence) a
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
toList :: Unbox a => Series k a -> [(k, a)]
{-# INLINE toList #-}
toList = G.toList


-- | Construct a 'Vector' of key-value pairs. The elements are in order sorted by key. 
toVector :: (Unbox a, Unbox k) => Series k a -> Vector (k, a)
{-# INLINE toVector #-}
toVector = G.toVector


-- | Construct a 'Series' from a 'Vector' of key-value pairs. There is no
-- condition on the order of pairs. Duplicate keys are silently dropped. If you
-- need to handle duplicate keys, see 'fromVectorDuplicates'.
--
-- Note that due to differences in sorting,
-- @Series.fromList@ and @Series.fromVector . Vector.fromList@ 
-- may not be equivalent if the input list contains duplicate keys.
fromVector :: (Ord k, Unbox k, Unbox a)
           => Vector (k, a) -> Series k a
{-# INLINE fromVector #-}
fromVector = G.fromVector


-- | Construct a series from a 'Vector' of key-value pairs.
-- Contrary to 'fromVector', values at duplicate keys are preserved. To keep each
-- key unique, an 'Occurrence' number counts up.
--
-- >>> import qualified Data.Vector.Unboxed as Unboxed
-- >>> let xs = fromVectorDuplicates $ Unboxed.fromList [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
-- >>> xs
--   index | values
--   ----- | ------
-- ('a',0) |      5
-- ('b',0) |      0
-- ('d',0) |      1
-- ('d',1) |     -4
-- ('d',2) |      7
fromVectorDuplicates :: (Unbox k, Unbox a, Ord k) => Vector (k, a) -> Series (k, Occurrence) a
{-# INLINE fromVectorDuplicates #-}
fromVectorDuplicates = G.fromVectorDuplicates


-- | Convert a series into a lazy @Map@.
toLazyMap :: (Unbox a) => Series k a -> ML.Map k a
{-# INLINE toLazyMap #-}
toLazyMap = G.toLazyMap


-- | Construct a series from a lazy @Map@.
fromLazyMap :: (Unbox a) => ML.Map k a -> Series k a
{-# INLINE fromLazyMap #-}
fromLazyMap = G.fromLazyMap


-- | Convert a series into a strict @Map@.
toStrictMap :: (Unbox a) => Series k a -> MS.Map k a
{-# INLINE toStrictMap #-}
toStrictMap = G.toStrictMap

-- | Construct a series from a strict @Map@.
fromStrictMap :: (Unbox a) => MS.Map k a -> Series k a
{-# INLINE fromStrictMap #-}
fromStrictMap = G.fromStrictMap


-- | \(O(n)\) Map every element of a 'Series'.
map :: (Unbox a, Unbox b) => (a -> b) -> Series k a -> Series k b
{-# INLINE map #-}
map = G.map


-- | \(O(n)\) Map every element of a 'Series', possibly using the key as well.
mapWithKey :: (Unbox a, Unbox b) => (k -> a -> b) -> Series k a -> Series k b
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
-- >>> xs `mapIndex` (take 1)
-- index | values
-- ----- | ------
--   "L" |      4
--   "P" |      1
mapIndex :: (Unbox a, Ord k, Ord g) => Series k a -> (k -> g) -> Series g a
{-# INLINE mapIndex #-}
mapIndex = G.mapIndex


-- | /O(n)/ Apply the monadic action to every element of a series and its
-- index, yielding a series of results.
mapWithKeyM :: (Unbox a, Unbox b, Monad m, Ord k) => (k -> a -> m b) -> Series k a -> m (Series k b)
{-# INLINE mapWithKeyM #-}
mapWithKeyM = G.mapWithKeyM


-- | /O(n)/ Apply the monadic action to every element of a series and its
-- index, discarding the results.
mapWithKeyM_ :: (Unbox a, Monad m) => (k -> a -> m b) -> Series k a -> m ()
{-# INLINE mapWithKeyM_ #-}
mapWithKeyM_ = G.mapWithKeyM_


-- | /O(n)/ Apply the monadic action to all elements of the series and their associated keys, 
-- yielding a series of results.
forWithKeyM :: (Unbox a, Unbox b, Monad m, Ord k) => Series k a -> (k -> a -> m b) -> m (Series k b)
{-# INLINE forWithKeyM #-}
forWithKeyM = G.forWithKeyM


-- | /O(n)/ Apply the monadic action to all elements of the series and their associated keys, 
-- discarding the results.
forWithKeyM_ :: (Unbox a, Monad m) => Series k a -> (k -> a -> m b) -> m ()
{-# INLINE forWithKeyM_ #-}
forWithKeyM_ = G.forWithKeyM_


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
takeWhile :: Unbox a => (a -> Bool) -> Series k a -> Series k a
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
dropWhile :: Unbox a => (a -> Bool) -> Series k a -> Series k a
dropWhile = G.dropWhile


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
--
-- >>> let xs = Series.fromList [ ('a', 0::Int),  ('b', 1),  ('g', 2) ]
-- >>> let ys = Series.fromList [ ('a', 10::Int), ('b', 11), ('d', 13) ]
-- >>> zipWithMatched (+) xs ys
-- index | values
-- ----- | ------
--   'a' |     10
--   'b' |     12
--
-- To combine elements where keys are in either series, see 'zipWith'.
zipWithMatched :: (Unbox a, Unbox b, Unbox c, Ord k) 
               => (a -> b -> c) -> Series k a -> Series k b -> Series k c
{-# INLINE zipWithMatched #-}
zipWithMatched = G.zipWithMatched


-- | Apply a function elementwise to three series, matching elements
-- based on their keys. Keys not present in all three series are dropped.
--
-- >>> let xs = Series.fromList [ ('a', 0::Int),  ('b', 1),   ('g', 2) ]
-- >>> let ys = Series.fromList [ ('a', 10::Int), ('b', 11),  ('d', 13) ]
-- >>> let zs = Series.fromList [ ('a', 20::Int), ('d', 13), ('e', 6) ]
-- >>> zipWithMatched3 (\x y z -> x + y + z) xs ys zs
-- index | values
-- ----- | ------
--   'a' |     30
zipWithMatched3 :: (Unbox a, Unbox b, Unbox c, Unbox d, Ord k) 
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
-- >>> import Data.Char ( ord )
-- >>> let xs = Series.fromList [ ('a', 0::Int), ('b', 1), ('c', 2) ]
-- >>> let ys = Series.fromList [ ('a', 10::Int), ('b', 11), ('d', 13) ]
-- >>> zipWithKey (\k x y -> ord k + x + y) xs ys
-- index | values
-- ----- | ------
--   'a' |    107
--   'b' |    110
--
-- To combine elements where keys are in either series, see 'zipWith'
zipWithKey :: (Unbox a, Unbox b, Unbox c, Unbox k, Ord k)  
           => (k -> a -> b -> c) -> Series k a -> Series k b -> Series k c
{-# INLINE zipWithKey #-}
zipWithKey = G.zipWithKey


-- | Apply a function elementwise to three series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
-- 
-- >>> import Data.Char ( ord )
-- >>> let xs = Series.fromList [ ('a', 0::Int), ('b', 1), ('g', 2) ]
-- >>> let ys = Series.fromList [ ('a', 10::Int), ('b', 11), ('d', 13) ]
-- >>> let zs = Series.fromList [ ('a', 20::Int), ('b', 7), ('d', 5) ]
-- >>> zipWithKey3 (\k x y z -> ord k + x + y + z) xs ys zs
-- index | values
-- ----- | ------
--   'a' |    127
--   'b' |    117
--
-- To combine elements where keys are in either series, see 'zipWith'
zipWithKey3 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox k, Ord k) 
            => (k -> a -> b -> c -> d) 
            -> Series k a 
            -> Series k b 
            -> Series k c
            -> Series k d
{-# INLINE zipWithKey3 #-}
zipWithKey3 = G.zipWithKey3


-- | Zip two 'Series' with a combining function, applying a 'ZipStrategy' when one key is present in one of the 'Series' but not both.
--
-- In the example below, we want to set the value to @-100@ (via @'constStrategy' (-100)@) for keys which are only present 
-- in the left 'Series', and drop keys (via 'skipStrategy') which are only present in the `right 'Series'  
--
-- >>> let xs = Series.fromList [ ('a', 0::Int),  ('b', 1),  ('g', 2) ]
-- >>> let ys = Series.fromList [ ('a', 10::Int), ('b', 11), ('d', 13) ]
-- >>> zipWithStrategy (+) (constStrategy (-100)) skipStrategy  xs ys
-- index | values
-- ----- | ------
--   'a' |     10
--   'b' |     12
--   'g' |   -100
--
-- Note that if you want to drop keys missing in either 'Series', it is faster to use @'zipWithMatched' f@ 
-- than using @'zipWithStrategy' f 'skipStrategy' 'skipStrategy'@.
zipWithStrategy :: (Ord k, Unbox a, Unbox b, Unbox c) 
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
zipWithStrategy3 :: (Ord k, Unbox a, Unbox b, Unbox c, Unbox d) 
                => (a -> b -> c -> d) -- ^ Function to combine values when present in all series
                -> ZipStrategy k a d  -- ^ Strategy for when the key is in the left series but not in all the others
                -> ZipStrategy k b d  -- ^ Strategy for when the key is in the center series but not in all the others
                -> ZipStrategy k c d  -- ^ Strategy for when the key is in the right series but not in all the others
                -> Series k a
                -> Series k b 
                -> Series k c
                -> Series k d
zipWithStrategy3 = G.zipWithStrategy3
{-# INLINE zipWithStrategy3 #-}


-- | Zip two 'Series' with a combining function. The value for keys which are missing from
-- either 'Series' is replaced with the appropriate `mempty` value.
--
-- >>> import Data.Monoid ( Sum(..) )
-- >>> let xs = Series.fromList [ ("2023-01-01", Sum (1::Int)), ("2023-01-02", Sum 2) ]
-- >>> let ys = Series.fromList [ ("2023-01-01", Sum (5::Int)), ("2023-01-03", Sum 7) ]
-- >>> zipWithMonoid (<>) xs ys
--        index |           values
--        ----- |           ------
-- "2023-01-01" | Sum {getSum = 6}
-- "2023-01-02" | Sum {getSum = 2}
-- "2023-01-03" | Sum {getSum = 7}
zipWithMonoid :: ( Monoid a, Monoid b
                 , Unbox a, Unbox b, Unbox c
                 , Ord k
                 ) 
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
esum :: (Ord k, Num a, Unbox a) 
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
eproduct :: (Ord k, Num a, Unbox a) 
         => Series k a 
         -> Series k a
         -> Series k a
eproduct = G.eproduct
{-# INLINE eproduct #-}


-- | \(O(n)\) Unzip a 'Series' of 2-tuples.
unzip :: (Unbox a, Unbox b) 
      => Series k (a, b)
      -> ( Series k a
         , Series k b
         )
unzip = G.unzip
{-# INLINE unzip #-}


-- | \(O(n)\) Unzip a 'Series' of 3-tuples.
unzip3 :: (Unbox a, Unbox b, Unbox c) 
       => Series k (a, b, c)
       -> ( Series k a
          , Series k b
          , Series k c
          )
unzip3 = G.unzip3
{-# INLINE unzip3 #-}


-- | Require a series to have a specific `Index`. 
-- Contrary to @select@, all keys in the `Index` will be present in the resulting series.
--
-- Note that unlike the implementation for boxed series (`Data.Series.require`), missing keys need to be mapped to some values because unboxed
-- series cannot contain values of type @`Maybe` a@. 
--
-- In the example below, the missing value for key @\"Taipei\"@ is mapped to 0:
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> require (const 0) xs (Index.fromList ["Paris", "Lisbon", "Taipei"])
--    index | values
--    ----- | ------
-- "Lisbon" |      4
--  "Paris" |      1
-- "Taipei" |      0
require :: (Unbox a, Ord k) 
        => (k -> a) -> Series k a -> Index k -> Series k a
{-# INLINE require #-}
require f = G.requireWith f id


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
-- Notice that the filtering is done on the values, not on the keys.
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
--
-- See also 'filterWithKey'.
filter :: (Unbox a, Ord k) => (a -> Bool) -> Series k a -> Series k a
{-# INLINE filter #-}
filter = G.filter


-- | Filter elements, taking into account the corresponding key. Only elements for which 
-- the predicate is @True@ are kept. 
filterWithKey :: (Unbox a, Ord k) 
              => (k -> a -> Bool) 
              -> Series k a 
              -> Series k a
{-# INLINE filterWithKey #-}
filterWithKey = G.filterWithKey


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
select :: (Unbox a, Selection s, Ord k) => Series k a -> s k -> Series k a
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
-- >>> xs `selectWhere` (Series.map (>1) xs)
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
selectWhere :: (Unbox a, Ord k) => Series k a -> Series k Bool -> Series k a
{-# INLINE selectWhere #-}
selectWhere = G.selectWhere


-- | \(O(\log n)\). Extract a single value from a series, by key.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs `at` "Paris"
-- Just 1
-- >>> xs `at` "Sydney"
-- Nothing
at :: (Unbox a, Ord k) => Series k a -> k -> Maybe a
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
iat :: Unbox a => Series k a -> Int -> Maybe a
{-# INLINE iat #-}
iat = G.iat


-- | \(O(n)\) Find the index of the maximum element in the input series.
-- If the input series is empty, 'Nothing' is returned.
--
-- The index of the first occurrence of the maximum element is returned.
--
-- >>> import qualified Data.Series.Unboxed as Series 
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
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
argmax :: (Ord a, Unbox a)
       => Series k a
       -> Maybe k
argmax = G.argmax


-- | \(O(n)\) Find the index of the minimum element in the input series.
-- If the input series is empty, 'Nothing' is returned.
--
-- The index of the first occurrence of the minimum element is returned.
-- >>> import qualified Data.Series.Unboxed as Series 
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
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
argmin :: (Ord a, Unbox a)
       => Series k a
       -> Maybe k
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
replace :: (Unbox a, Ord k) => Series k a -> Series k a -> Series k a
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
(|->) :: (Unbox a, Ord k) => Series k a -> Series k a -> Series k a
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
(<-|) :: (Unbox a, Ord k) => Series k a -> Series k a -> Series k a
{-# INLINE (<-|) #-}
(<-|) = (G.<-|)


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
aggregateWith :: (Ord k, Ord g, Unbox a, Unbox b) 
              => Grouping k g a 
              -> (Series k a -> b) 
              -> Series g b
{-# INLINE aggregateWith #-}
aggregateWith = G.aggregateWith


-- | Aggregate each group in a 'Grouping' using a binary function.
-- While this is not as expressive as 'aggregate', users looking for maximum
-- performance should use 'foldWith' as much as possible.
foldWith :: (Ord g, Unbox a) 
         => Grouping k g a
         -> (a -> a -> a)
         -> Series g a
{-# INLINE foldWith #-}
foldWith = G.foldWith


-- | Expanding window aggregation.
--
-- >>> :{ 
--     let (xs :: Series Int Int) 
--          = fromList [ (1, 0)
--                     , (2, 1)
--                     , (3, 2)
--                     , (4, 3)
--                     , (5, 4)
--                     , (6, 5)
--                     ]
--     in (xs `expanding` sum) :: Series Int Int 
-- :}
-- index | values
-- ----- | ------
--     1 |      0
--     2 |      1
--     3 |      3
--     4 |      6
--     5 |     10
--     6 |     15
expanding :: (Unbox a, Unbox b) 
          => Series k a        -- ^ Series vector
          -> (Series k a -> b) -- ^ Aggregation function
          -> Series k b        -- ^ Resulting vector
{-# INLINE expanding #-}
expanding = G.expanding


-- | General-purpose window aggregation.
--
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
windowing :: (Ord k, Unbox a, Unbox b)
          => (k -> Range k)
          -> (Series k a -> b)
          -> Series k a
          -> Series k b
{-# INLINE windowing #-}
windowing = G.windowing 


-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results.
foldMap :: (Monoid m, Unbox a) => (a -> m) -> Series k a -> m
{-# INLINE foldMap #-}
foldMap f = Vector.foldMap f . values


-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
foldMap' :: (Monoid m, Unbox a) => (a -> m) -> Series k a -> m
{-# INLINE foldMap' #-}
foldMap' f = Vector.foldMap' f . values


-- | /O(n)/ Check if all elements satisfy the predicate.
all :: Unbox a => (a -> Bool) -> Series k a -> Bool
{-# INLINE all #-}
all f = Vector.all f . values


-- | /O(n)/ Check if any element satisfies the predicate.
any :: Unbox a => (a -> Bool) -> Series k a -> Bool
{-# INLINE any #-}
any f = Vector.any f . values


-- | /O(n)/ Check if all elements are 'True'.
and :: Series k Bool -> Bool
{-# INLINE and #-}
and = Vector.and . values


-- | /O(n)/ Check if any element is 'True'.
or :: Series k Bool -> Bool
{-# INLINE or #-}
or = Vector.or . values


-- | /O(n)/ Compute the sum of the elements.
sum :: (Unbox a, Num a) => Series k a -> a
{-# INLINE sum #-}
sum = Vector.sum . values


-- | /O(n)/ Compute the product of the elements.
product :: (Unbox a, Num a) => Series k a -> a
{-# INLINE product #-}
product = Vector.product . values


-- | /O(n)/ Yield the maximum element of the series. The series may not be
-- empty. In case of a tie, the first occurrence wins.
maximum :: (Unbox a, Ord a) => Series k a -> a
{-# INLINE maximum #-}
maximum = Vector.maximum . values


-- | /O(n)/ Yield the minimum element of the series. The series may not be
-- empty. In case of a tie, the first occurrence wins.
minimum :: (Unbox a, Ord a) => Series k a -> a
{-# INLINE minimum #-}
minimum = Vector.minimum . values


-- | Compute the mean of the values in the series.
-- An empty series will have a mean of NaN.
mean :: (Unbox a, Real a, RealFloat b) => Series k a -> b
{-# INLINE mean #-}
mean = G.mean


-- | Compute the mean and variance of the values in a series in a single-pass.
meanAndVariance :: (Unbox a, RealFloat a) => Series k a -> (a, a)
{-# INLINE meanAndVariance #-}
meanAndVariance = G.meanAndVariance


-- | Population variance.
var :: (Unbox a, RealFloat a) => Series k a -> a
{-# INLINE var #-}
var = G.var


-- | Population standard deviation.
std :: (Unbox a, RealFloat a) => Series k a -> a
{-# INLINE std #-}
std = sqrt . var


-- | Sample variance.
sampleVariance :: (Unbox a, RealFloat a) => Series k a -> a
{-# INLINE sampleVariance #-}
sampleVariance = G.sampleVariance
