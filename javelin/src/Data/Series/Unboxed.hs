-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Series.Unboxed
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
-- This module contains data structures and functions to work with `Series` capable of holding unboxed values,
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
-- A `Series` of type @Series k a@ is a labeled array of values of type @a@,
-- indexed by keys of type @k@.
--
-- Like `Data.Map.Strict.Map` from the @containers@ package, `Series` support efficient:
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

    -- * Building/converting `Series`
    singleton, fromIndex,
    -- ** Lists
    fromList, fromListDuplicates, Occ, toList,
    -- ** Strict Maps
    fromStrictMap, toStrictMap,
    -- ** Lazy Maps
    fromLazyMap, toLazyMap,
    -- ** Conversion between `Series` types
    G.convert,

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, null, length,
    takeWhile, dropWhile, filter,

    -- * Combining series
    zipWithMatched, 

    -- * Index manipulation
    require, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, Selection, 
    -- ** Single-element access
    at, iat, 

    -- * Replacement
    replace, (|->), (<-|),

    -- * Grouping operations
    GroupBy, groupBy, aggregateWith,

    -- * Folds
    -- ** General folds
    foldMap, foldMap', 
    -- ** Specialized folds
    all, any, and, or, sum, product, maximum, minimum,

    -- * aggregation
    first, last,
    mean, var, std, 
    sampleVariance,
    meanAndVariance,
) where

import qualified Data.Map.Lazy       as ML
import qualified Data.Map.Strict     as MS
import           Data.Series.Index   ( Index )
import           Data.Series.Generic.View 
                                     ( Range, Selection, to )
import           Data.Series.Generic ( Occ )
import qualified Data.Series.Generic as G
import           Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as Vector

import           Prelude             hiding ( map, zipWith, filter, foldMap, all, any, and, or
                                            , sum, product, maximum, minimum, takeWhile, dropWhile
                                            , last
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


-- | Create a `Series` with a single element.
singleton :: Unbox a => k -> a -> Series k a
{-# INLINE singleton #-}
singleton = G.singleton


-- | \(O(n)\) Generate a `Series` by mapping every element of its index.
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
fromList = fromStrictMap . MS.fromList


-- | Construct a series from a list of key-value pairs.
-- Contrary to `fromList`, values at duplicate keys are preserved. To keep each
-- key unique, an `Occ` (short for occurrence) number counts up.
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
fromListDuplicates :: (Unbox a, Ord k) => [(k, a)] -> Series (k, Occ) a
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


-- | \(O(n)\) Map every element of a `Series`.
map :: (Unbox a, Unbox b) => (a -> b) -> Series k a -> Series k b
{-# INLINE map #-}
map = G.map


-- | \(O(n)\) Map every element of a `Series`, possibly using the key as well.
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
-- >>> xs `mapIndex` head
-- index | values
-- ----- | ------
--   'L' |      4
--   'P' |      1
mapIndex :: (Unbox a, Ord k, Ord g) => Series k a -> (k -> g) -> Series g a
{-# INLINE mapIndex #-}
mapIndex = G.mapIndex


-- | \(O(n)\) Returns the longest prefix (possibly empty) of the input `Series` that satisfy a predicate.
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
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithMatched (+) xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
--
-- To combine elements where keys are in either series, see `zipWith`.
zipWithMatched :: (Unbox a, Unbox b, Unbox c, Ord k) 
               => (a -> b -> c) -> Series k a -> Series k b -> Series k c
{-# INLINE zipWithMatched #-}
zipWithMatched = G.zipWithMatched


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
filter :: (Unbox a, Ord k) => (a -> Bool) -> Series k a -> Series k a
{-# INLINE filter #-}
filter = G.filter


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


-- | Data type representing groups of @Series k a@, indexed by keys of type @g@.
-- See the documentation for @groupBy@.
type GroupBy = G.GroupBy Vector

-- | Group values in a `Series` by some function (@k -> g@).
--
-- This function is expected to be used in conjunction with `aggregateWith`:
-- 
-- >>> type Date = (Int, String)
-- >>> month :: (Date -> String) = snd
-- >>> :{ 
--     let (xs :: Series Date Int) 
--              = Series.fromList [ ((2020, "January"),  0)
--                                , ((2021, "January"), -5)
--                                , ((2020, "June")   , 20)
--                                , ((2021, "June")   , 25) 
--                                ]
--      in xs `groupBy` month `aggregateWith` Series.minimum
-- :}
--     index | values
--     ----- | ------
-- "January" |     -5
--    "June" |     20
groupBy :: (Unbox a, Ord k, Ord g) 
        => Series k a       -- ^ Input series
        -> (k -> g)         -- ^ Grouping function
        -> GroupBy g k a    -- ^ Grouped series
{-# INLINE groupBy #-}
groupBy = G.groupBy


-- | Aggregate grouped series. This function is expected to be used in conjunction
-- with `groupBy`.
aggregateWith :: (Unbox b) 
              => GroupBy g k a      -- ^ Grouped series
              -> (Series k a -> b)  -- ^ Aggregation function
              -> Series g b         -- ^ Aggregated series
{-# INLINE aggregateWith #-}
aggregateWith = G.aggregateWith


-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results.
foldMap :: (Monoid m, Unbox a) => (a -> m) -> Series k a -> m
{-# INLINE foldMap #-}
foldMap f = Vector.foldMap f . values


-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
-- Note that it's implemented in terms of 'foldl'', so it fuses in most
-- contexts.
--
-- @since 0.12.2.0
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


-- | Extract the first value out of a `Series`.
first :: Unbox a => Series k a -> a
{-# INLINE first #-}
first = G.first


-- | Extract the last value out of a `Series`.
last :: Unbox a => Series k a -> a
{-# INLINE last #-}
last = G.last


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
