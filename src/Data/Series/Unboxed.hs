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
    -- ** Lists
    fromList, toList,
    -- ** Strict Maps
    fromStrictMap, toStrictMap,
    -- ** Lazy Maps
    fromLazyMap, toLazyMap,
    -- ** Conversion between `Series` types
    G.convert,

    -- * IO operations
    G.ColumnName, columns, columnsFromFile,
    -- ** From CSV
    readCSV, readCSVFromFile,
    -- ** From JSON
    readJSON, readJSONFromFile,

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, filter,

    -- * Combining series
    zipWithMatched, 

    -- * Index manipulation
    dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, Selection, 
    -- ** Single-element access
    at, iat, 

    -- * Grouping operations
    GroupBy, groupBy, aggregateWith,

    -- * Folds
    -- ** General folds
    foldMap, foldMap', 
    -- ** Specialized folds
    all, any, and, or, sum, product, maximum, minimum,

    -- * Numerical aggregation
    mean, var, std, 
    sampleVariance,
    meanAndVariance,
) where

import           Data.Aeson          ( FromJSON, FromJSONKey )
import qualified Data.ByteString.Lazy as BL
import           Data.Csv            ( FromField )
import qualified Data.Map.Lazy       as ML
import qualified Data.Map.Strict     as MS
import           Data.Series.Index   ( Index )
import           Data.Series.Generic.View 
                                     ( Range, Selection, to )
import qualified Data.Series.Generic as G
import           Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as Vector

import           Prelude             hiding ( map, zipWith, filter, foldMap, all, any, and, or
                                            , sum, product, maximum, minimum 
                                            )

-- $setup
-- >>> import qualified Data.Series.Unboxed as Series
-- >>> import qualified Data.Series.Index as Index

infixl 1 `select` 

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
fromList :: (Unbox a, Ord k) => [(k, a)] -> Series k a
{-# INLINE fromList #-}
fromList = fromStrictMap . MS.fromList


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
toLazyMap :: (Unbox a, Ord k) => Series k a -> ML.Map k a
{-# INLINE toLazyMap #-}
toLazyMap = G.toLazyMap


-- | Construct a series from a lazy @Map@.
fromLazyMap :: (Unbox a, Ord k) => ML.Map k a -> Series k a
{-# INLINE fromLazyMap #-}
fromLazyMap = G.fromLazyMap


-- | Convert a series into a strict @Map@.
toStrictMap :: (Unbox a, Ord k) => Series k a -> MS.Map k a
{-# INLINE toStrictMap #-}
toStrictMap = G.toStrictMap

-- | Construct a series from a strict @Map@.
fromStrictMap :: (Unbox a, Ord k) => MS.Map k a -> Series k a
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
-- See `reindex` if you want to ensure that all keys are present.
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
aggregateWith :: (Unbox b, Ord g) 
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


readCSV :: (Unbox a, Ord k, FromField k, FromField a)
        => G.ColumnName -- ^ Index column
        -> G.ColumnName -- ^ Values volumn
        -> BL.ByteString
        -> Either String (Series k a)
readCSV = G.readCSV


readCSVFromFile :: (Unbox a, Ord k, FromField k, FromField a)
                => FilePath
                -> G.ColumnName -- ^ Index column
                -> G.ColumnName -- ^ Values column
                -> IO (Either String (Series k a))
readCSVFromFile = G.readCSVFromFile 


columns :: BL.ByteString -> Either String [G.ColumnName]
columns = G.columns


columnsFromFile :: FilePath -> IO (Either String [G.ColumnName])
columnsFromFile = G.columnsFromFile


readJSON :: (Unbox a, Ord k, FromJSONKey k, FromJSON a) 
         => BL.ByteString 
         -> Either String (MS.Map G.ColumnName (Series k a))
readJSON = G.readJSON


readJSONFromFile :: (Unbox a, Ord k, FromJSONKey k, FromJSON a) 
                 => FilePath 
                 -> IO (Either String (MS.Map G.ColumnName (Series k a)))
readJSONFromFile = G.readJSONFromFile




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
