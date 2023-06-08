module Data.Series.Generic.View (
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
    require,
    requireWith,
    filter,
    dropna,
    dropIndex,

    -- * Creating and accessing ranges
    Range(..),
    to,
) where

import           Data.Series.Index      ( Index )
import qualified Data.Series.Index      as Index
import           Data.Maybe             ( fromJust, isJust )
import           Data.Series.Generic.Definition ( Series(..) )
import qualified Data.Series.Generic.Definition as G
import           Data.Set               ( Set )
import qualified Data.Set               as Set
import qualified Data.Vector            as Boxed
import           Data.Vector.Generic    ( Vector )
import qualified Data.Vector.Generic    as Vector

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
(!) :: Vector v a => Series v k a -> Int -> a
(MkSeries _ vs) ! ix = (Vector.!) vs ix


-- | \(O(\log n)\). Extract a single value from a series, by key.
at :: (Vector v a, Ord k) => Series v k a -> k -> Maybe a
at (MkSeries ks vs) k = do
    ix <- Index.lookupIndex k ks
    pure $ Vector.unsafeIndex vs ix 
{-# INLINE at #-}


-- | \(O(1)\). Extract a single value from a series, by index.
iat :: Vector v a => Series v k a -> Int -> Maybe a
iat (MkSeries _ vs) =  (Vector.!?) vs
{-# INLINE iat #-}


-- | require a series with a new index.
-- Contrary to `select`, all keys in @Set k@ will be present in the re-indexed series.
require :: (Vector v a, Vector v (Maybe a), Ord k) 
        => Series v k a -> Index k -> Series v k (Maybe a)
{-# INLINE require #-}
require = requireWith (const Nothing) Just


-- | Generalization of `require`, which maps missing keys to values.
-- This is particularly useful for `Vector` instances which don't support `Maybe`, like "Data.Vector.Unboxed".
requireWith :: (Vector v a, Vector v b, Ord k)
            => (k -> b)  -- ^ Function to apply to keys which are missing from the input series, but required in the input index
            -> (a -> b)  -- ^ Function to apply to values which are in the input series and input index.
            -> Series v k a 
            -> Index k 
            -> Series v k b
{-# INLINE requireWith #-}
requireWith replacement f xs ss 
    = let existingKeys = index xs `Index.intersection` ss
          newKeys      = ss `Index.difference` existingKeys
       in G.map f (xs `select` existingKeys) <> MkSeries newKeys (Vector.fromListN (Index.size newKeys) (replacement <$> Index.toAscList newKeys))


-- | Drop the index of a series by replacing it with an @Int@-based index. Values will
-- be indexed from 0.
dropIndex :: Series v k a -> Series v Int a
dropIndex (MkSeries ks vs) = MkSeries (Index.fromDistinctAscList [0..Index.size ks - 1]) vs


-- | Filter elements. Only elements for which the predicate is @True@ are kept. 
-- Notice that the filtering is done on the values, not on the keys
filter :: (Vector v a, Vector v Int, Ord k) 
       => (a -> Bool) -> Series v k a -> Series v k a
{-# INLINE filter #-}
filter predicate xs@(MkSeries ks vs) 
    = let indicesToKeep = Vector.findIndices predicate vs
          keysToKeep = Index.fromList [Index.elemAt ix ks | ix <- Vector.toList indicesToKeep]
       in xs `select` keysToKeep


-- | Drop elements which are not available (NA). 
dropna :: (Vector v a, Vector v (Maybe a), Vector v Int, Ord k) 
       => Series v k (Maybe a) -> Series v k a
{-# INLINE dropna #-}
dropna = G.map fromJust . filter isJust


-- | Datatype representing an /inclusive/ range of keys.
-- The canonical way to construct a @Range@ is to use @to@:
--
-- >>> 'a' `to` 'z'
-- Range (from 'a' to 'z')
--
-- A @Range@ can be used to efficiently select a sub-series with `select`.
data Range k = MkRange k k

instance Show k => Show (Range k) where
    show :: Range k -> String
    show (MkRange start stop) = mconcat ["Range (from ", show start, " to ", show stop, ")"]


-- | Find the keys which are in range. In case of an empty `Series`,
-- the returned value is `Nothing`.
keysInRange :: Ord k => Series v k a -> Range k -> Maybe (k, k)
{-# INLINE keysInRange #-}
keysInRange (MkSeries ks _) (MkRange start stop)
    | Index.null ks = Nothing
    | otherwise     = let (_, afterStart) = Set.spanAntitone (< start) $ Index.toSet ks
                          inRange         = Set.takeWhileAntitone (<= stop) afterStart
                       in if Set.null inRange 
                            then Nothing
                            else Just (Set.findMin inRange, Set.findMax inRange)


-- | Create a @Range@ which can be used for slicing. This function
-- is expected to be used in conjunction with `select`: 
to :: Ord k => k -> k -> Range k
to k1 k2 = MkRange (min k1 k2) (max k1 k2)


-- | Class for datatypes which can be used to select sub-series using `select`.
--
-- There are two use-cases for `select`:
--
--  * Bulk random-access (selecting from a @Set@ of keys);
--  * Bulk ordered access (selecting from a @Range@ of keys).
--
-- See the documentation for `select`.
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
    -- Note that with `select`, you'll always get a sub-series; if you ask for a key which is not
    -- in the series, it'll be ignored:
    --
    -- >>> xs `select` Index.fromList ['a', 'd', 'e']
    -- index | values
    -- ----- | ------
    --   'a' |     10
    --   'd' |     40
    --
    -- See @require@ if you want to ensure that all keys are present.
    select :: (Vector v a, Ord k) => Series v k a -> s k -> Series v k a


instance Selection Index where
    -- | Select all keys in `Index` from a series. Keys which are not
    -- in the series are ignored.
    select :: (Vector v a, Ord k) => Series v k a -> Index k -> Series v k a
    {-# INLINE select #-}
    select (MkSeries ks vs) ss 
        = let selectedKeys = ks `Index.intersection` ss
            -- Surprisingly, using `Vector.backpermute` does not
            -- perform as well as `Vector.map (Vector.unsafeIndex vs)`
            -- for large Series
           in MkSeries selectedKeys $ Vector.convert
                                    $ Boxed.map (Vector.unsafeIndex vs) 
                                    $ Boxed.map (`Index.findIndex` ks) 
                                    $ Index.toAscVector selectedKeys


-- | Selecting a sub-series from a `Set` is a convenience
-- function. Internally, the `Set` is converted to an index first.
instance Selection Set where
    select :: (Vector v a, Ord k) => Series v k a -> Set k -> Series v k a
    select xs = select xs . Index.fromSet


-- | Selecting a sub-series from a list is a convenience
-- function. Internally, the list is converted to an index first.
instance Selection [] where
    select :: (Vector v a, Ord k) => Series v k a -> [k] -> Series v k a
    select xs = select xs . Index.fromList


-- | Selecting a sub-series based on a @Range@ is most performant.
-- Constructing a @Range@ is most convenient using the `to` function.
instance Selection Range where
    select :: (Vector v a, Ord k) => Series v k a -> Range k -> Series v k a
    {-# INLINE select #-}
    select series rng = case keysInRange series rng of 
        Nothing              -> mempty
        Just (kstart, kstop) -> let indexOf xs k = Index.findIndex k (index xs)
                                 in slice (series `indexOf` kstart) (1 + indexOf series kstop) series


-- | Select a sub-series from a series matching a condition.
selectWhere :: (Vector v a, Vector v Int, Vector v Bool, Ord k) => Series v k a -> Series v k Bool -> Series v k a
{-# INLINE selectWhere #-}
selectWhere xs ys = xs `select` Index.fromSet keysWhereTrue
    where
        (MkSeries _ cond) = ys `select` index xs
        whereValuesAreTrue = Set.fromDistinctAscList $ Vector.toList (Vector.findIndices id cond)
        keysWhereTrue = Set.mapMonotonic (`Index.elemAt` index xs) whereValuesAreTrue


-- | Yield a subseries based on indices. The end index is not included.
slice :: Vector v a
      => Int -- ^ Start index
      -> Int -- ^ End index, which is not included
      -> Series v k a 
      -> Series v k a
{-# INLINE slice #-}
slice start stop (MkSeries ks vs) 
    = let stop' = min (Vector.length vs) stop
    in MkSeries { index  = Index.take (stop' - start) $ Index.drop start ks
                , values = Vector.slice start (stop' - start) vs
                }
