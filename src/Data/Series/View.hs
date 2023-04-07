{-# LANGUAGE RecordWildCards #-}
module Data.Series.View (
    -- * Accessing a single element
    (!),
    at,
    iat,

    -- * Bulk access
    select,
    slice,
    selectWhere,
    Selection,

    -- * Resizing
    reindex,
    filter,
    dropna,
    mapIndex,
    dropIndex,

    -- * Creating and accessing ranges
    Range(..),
    to,
) where

import           Data.Series.Index      ( Index )
import qualified Data.Series.Index      as Index
import           Data.Maybe             ( fromJust, isJust )
import qualified Data.Map.Strict        as Map
import           Data.Series.Definition ( Series(..), fromStrictMap )
import           Data.Set               ( Set )
import qualified Data.Set               as Set
import qualified Data.Vector            as Vector

import           Prelude                hiding ( filter )

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Series.Index as Index 

infixr 9 `to` -- Ensure that @to@ binds strongest
infixl 1 `select` 


-- | \(O(1)\). Extract a single value from a series, by index. 
-- An exception is thrown if the index is out-of-bounds.
--
-- A safer alternative is @iat@, which returns @Nothing@ if the index is
-- out-of-bounds.
(!) :: Series k a -> Int -> a
(MkSeries _ vs) ! ix = (Vector.!) vs ix


-- | \(O(\log n)\). Extract a single value from a series, by key.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs `at` "Paris"
-- Just 1
-- >>> xs `at` "Sydney"
-- Nothing
at :: Ord k => Series k a -> k -> Maybe a
at (MkSeries ks vs) k = do
    ix <- Index.lookupIndex k ks
    pure $ Vector.unsafeIndex vs ix 
{-# INLINE at #-}


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
iat (MkSeries _ vs) =  (Vector.!?) vs
{-# INLINE iat #-}


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
mapIndex MkSeries{..} f
    -- Note that the order in which items are kept appears to be backwards;
    -- See the examples for Data.Map.Strict.fromListWith
    = let mapping   = Map.fromListWith (\_ x -> x) $ [(f k, k) | k <- Index.toAscList index]
          newvalues = fmap (\k -> values Vector.! Index.findIndex k index) mapping
       in fromStrictMap newvalues


-- | Reindex a series with a new index.
-- Contrary to @select@, all keys in @Set k@ will be present in the re-indexed series.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> xs `reindex` Index.fromList ["Paris", "Lisbon", "Taipei"]
--    index |  values
--    ----- |  ------
-- "Lisbon" |  Just 4
--  "Paris" |  Just 1
-- "Taipei" | Nothing
reindex :: Ord k => Series k a -> Index k -> Series k (Maybe a)
{-# INLINE reindex #-}
reindex xs ss 
    = let existingKeys = index xs `Index.intersection` ss
          newKeys      = ss `Index.difference` existingKeys
       in (Just <$> (xs `select` existingKeys)) <> MkSeries newKeys (Vector.replicate (Index.size newKeys) Nothing)


-- | Drop the index of a series by replacing it with an @Int@-based index. Values will
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
dropIndex (MkSeries ks vs) = MkSeries (Index.fromAscList [0..Index.size ks - 1]) vs


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
filter predicate xs@(MkSeries ks vs) 
    = let indicesToKeep = Vector.findIndices predicate vs
          keysToKeep = Index.fromList [Index.elemAt ix ks | ix <- Vector.toList indicesToKeep]
       in xs `select` keysToKeep


-- | Drop elements which are not available (NA). 
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> let ys = xs `reindex` Index.fromList ["Paris", "London", "Lisbon", "Toronto"]
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
dropna = fmap fromJust . filter isJust


-- | Datatype representing an /inclusive/ range of keys.
-- The canonical way to construct a @Range@ is to use @to@:
--
-- >>> 'a' `to` 'z'
-- Range (from 'a' to 'z')
--
-- A @Range@ can be used to efficiently select a sub-series with @select@.
data Range k = MkRange k k

instance Show k => Show (Range k) where
    show :: Range k -> String
    show (MkRange start stop) = mconcat ["Range (from ", show start, " to ", show stop, ")"]


-- | Find the keys which are in range
keysInRange :: Ord k => Series k a -> Range k -> (k, k)
{-# INLINE keysInRange #-}
keysInRange (MkSeries ks _) (MkRange start stop)
    = let (_, afterStart) = Set.spanAntitone (< start) $ Index.toSet ks
          inRange         = Set.takeWhileAntitone (<= stop) afterStart
       in (Set.findMin inRange, Set.findMax inRange)


-- | Create a @Range@ which can be used for slicing. This function
-- is expected to be used in conjunction with @select@: 
--
-- >>> let xs = Series.fromList [('a', 10::Int), ('b', 20), ('c', 30), ('d', 40)]
-- >>> xs `select` 'b' `to` 'c'
-- index | values
-- ----- | ------
--   'b' |     20
--   'c' |     30
to :: Ord k => k -> k -> Range k
to k1 k2 = MkRange (min k1 k2) (max k1 k2)


-- | Class for datatypes which can be used to select sub-series using @select@.
--
-- There are two use-cases for @select@:
--
--  * Bulk random-access (selecting from a @Set@ of keys);
--  * Bulk ordered access (selecting from a @Range@ of keys).
--
-- See the documentation for @select@.
class Selection s where
    -- | Select a subseries. There are two main ways to do this.
    --
    -- The first way to do this is to select a sub-series based on keys:
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
    -- Note that with @select@, you'll always get a sub-series; if you ask for a key which is not
    -- in the series, it'll be ignored:
    --
    -- >>> xs `select` Index.fromList ['a', 'd', 'e']
    -- index | values
    -- ----- | ------
    --   'a' |     10
    --   'd' |     40
    --
    -- See @reindex@ if you want to ensure that all keys are present.
    select :: Ord k => Series k a -> s k -> Series k a


instance Selection Index where
    -- | Select all keys in `Index` from a series. Keys which are not
    -- in the series are ignored.
    select :: Ord k => Series k a -> Index k -> Series k a
    {-# INLINE select #-}
    select (MkSeries ks vs) ss 
        = let selectedKeys = ks `Index.intersection` ss
            -- Surprisingly, using `Vector.backpermute` does not
            -- perform as well as `Vector.map (Vector.unsafeIndex vs)`
            -- for large Series
              newValues = Vector.map (Vector.unsafeIndex vs) 
                        $ Vector.map (`Index.findIndex` ks) 
                        $ Vector.fromListN (Index.size selectedKeys) 
                                           (Index.toAscList selectedKeys)
           in MkSeries selectedKeys newValues


-- | Selecting a sub-series from a `Set` is a convenience
-- function. Internally, the `Set` is converted to an index first.
--
-- >>> let xs = Series.fromList [('a', 10::Int), ('b', 20), ('c', 30), ('d', 40)]
-- >>> xs
-- index | values
-- ----- | ------
--   'a' |     10
--   'b' |     20
--   'c' |     30
--   'd' |     40
-- >>> let keys = Set.fromList ['a', 'd', 'e']
-- >>> xs `select` keys
-- index | values
-- ----- | ------
--   'a' |     10
--   'd' |     40
instance Selection Set where
    select :: Ord k => Series k a -> Set k -> Series k a
    select xs = select xs . Index.fromSet


-- | Selecting a sub-series from a list is a convenience
-- function. Internally, the list is converted to an index first.
--
-- >>> let xs = Series.fromList [('a', 10::Int), ('b', 20), ('c', 30), ('d', 40)]
-- >>> xs
-- index | values
-- ----- | ------
--   'a' |     10
--   'b' |     20
--   'c' |     30
--   'd' |     40
-- >>> xs `select` ['a', 'd', 'e']
-- index | values
-- ----- | ------
--   'a' |     10
--   'd' |     40
instance Selection [] where
    select :: Ord k => Series k a -> [k] -> Series k a
    select xs = select xs . Index.fromList


-- | Selecting a sub-series based on a @Range@ is most performant.
-- Constructing a @Range@ is most convenient using the `to` function like so:
-- 
-- >>> let xs = Series.fromList [('a', 10::Int), ('b', 20), ('c', 30), ('d', 40)]
-- >>> xs
-- index | values
-- ----- | ------
--   'a' |     10
--   'b' |     20
--   'c' |     30
--   'd' |     40
-- >>> xs `select` 'b' `to` 'c'
-- index | values
-- ----- | ------
--   'b' |     20
--   'c' |     30
instance Selection Range where
    select :: Ord k => Series k a -> Range k -> Series k a
    {-# INLINE select #-}
    select series rng 
        = let (kstart, kstop) = keysInRange series rng 
              indexOf xs k = Index.findIndex k (index xs)
           in slice (series `indexOf` kstart) (1 + indexOf series kstop) series


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
--
-- @selectWhere@ can be used in combinations with broadcasting operators to
-- make selections more readable:
--
-- >>> import Data.Series ( (/=|) )
-- >>> let threshold = 1 :: Int
-- >>> xs `selectWhere` (xs /=| threshold)
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
selectWhere :: Ord k => Series k a -> Series k Bool -> Series k a
selectWhere xs ys = xs `select` (Index.fromSet keysWhereTrue)
    where
        (MkSeries _ cond) = ys `select` index xs
        whereValuesAreTrue = Set.fromDistinctAscList $ Vector.toList (Vector.findIndices id cond)
        keysWhereTrue = Set.mapMonotonic (`Index.elemAt` index xs) whereValuesAreTrue


-- | Yield a subseries based on indices. The end index is not included.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> slice 0 2 xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
slice :: Int -- ^ Start index
      -> Int -- ^ End index, which is not included
      -> Series k a 
      -> Series k a
{-# INLINE slice #-}
slice start stop (MkSeries ks vs) 
    = let stop' = min (length vs) stop
    in MkSeries { index  = Index.take (stop' - start) $ Index.drop start ks
                , values = Vector.slice start (stop' - start) vs
                }
